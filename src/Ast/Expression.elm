module Ast.Expression exposing
    ( Expression(..)
    , expression
    , ExportSet(..)
    , Type(..)
    , Statement(..)
    , Function(..)
    , LetBinding(..)
    , Parameter(..)
    , statement
    , statements
    , infixStatements
    , opTable )

{-| This module exposes parsers for Elm expressions.

# Types
@docs Expression

# Parsers
@docs expression

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import Dict exposing (Dict)
import List exposing (singleton)
import List.Extra exposing (break)
import String

import Ast.BinOp exposing (..)
import Ast.Helpers exposing (..)

type Collect a
  = Cont a
  | Stop a

{-| Representations for Elm's expressions. -}
type Expression
  = Character Char
  | String String
  | Integer Int
  | Float Float
  | Variable (List Name)
  | TupleExpr (List Expression)
  | OperatorReference String
  | List (List Expression)
  | Access Expression (List Name)
  | Record (List (Name, Expression))
  | RecordUpdate Name (List (Name, Expression))
  | If Expression Expression Expression
  | Let (List LetBinding) Expression
  | Case Expression (List (Expression, Expression))
  | Lambda (List Parameter) Expression
  | Application Expression Expression
  | BinOp Expression Expression Expression

character : Parser s Expression
character =
  Character <$> between_ (char '\'') anyChar

string : Parser s Expression
string =
  let
    singleString =
      String
        <$> (Combine.string "\"" *> regex "(\\\\\"|[^\"\n])*" <* Combine.string "\"")

    multiString  =
      (String << String.concat)
        <$> (Combine.string "\"\"\"" *> many (regex "[^\"]*") <* Combine.string "\"\"\"")
  in
    multiString <|> singleString

integer : Parser s Expression
integer =
  Integer <$> Combine.Num.int

float : Parser s Expression
float =
  Float <$> Combine.Num.float

access : Parser s Expression
access =
  Access <$> variable <*> many1 (Combine.string "." *> loName)

variable : Parser s Expression
variable =
  Variable <$> choice [ singleton <$> loName
                      , sepBy1 (Combine.string "." ) upName
                      ]

list : OpTable -> Parser s Expression
list ops =
  lazy <| \() ->
    List <$> brackets (commaSeparated_ (expression ops))


record : OpTable -> Parser s Expression
record ops =
  lazy <| \() ->
    Record <$> braces (commaSeparated_ ((,) <$> loName <*> (symbol "=" *> expression ops)))


tuple : OpTable -> Parser s Expression
tuple ops =
    lazy (\_ ->
        TupleExpr <$> (parens <| commaSeparated (expression ops))
    )


letExpression : OpTable -> Parser s Expression
letExpression ops =
    lazy <| \() ->
      Let
        <$> (symbol "let" *> (many <| between_ whitespace (
                choice
                    [ FunctionBinding <$> functionDeclaration ops
                    , DestructuringBinding
                        <$> (functionParameter ops)
                        <*> (symbol "=" *> expression ops)
                    ]
            )))
        <*> (symbol "in" *> expression ops)


ifExpression : OpTable -> Parser s Expression
ifExpression ops =
  lazy <| \() ->
    If
      <$> (symbol "if" *> expression ops)
      <*> (symbol "then" *> expression ops)
      <*> (symbol "else" *> expression ops)

caseExpression : OpTable -> Parser s Expression
caseExpression ops =
  let
    binding =
      lazy <| \() ->
        (,)
          <$> (whitespace *> expression ops)
          <*> (symbol "->" *> expression ops)
  in
    lazy <| \() ->
      Case
        <$> (symbol "case" *> expression ops)
        <*> (symbol "of" *> many1 binding)

lambda : OpTable -> Parser s Expression
lambda ops =
  lazy <| \() ->
    Lambda
      <$> (symbol "\\" *> many (between_ spaces (functionParameter ops)))
      <*> (symbol "->" *> expression ops)

{- Parse function application.
   Parse function arguments as long as _beginning_ of next expression
   is indented more than function name.
-}
application : OpTable -> Parser s Expression
application ops =
  lazy <| \() ->
    withLocation (\location ->
        term ops |> chainl (Application <$
            ( lookAhead (whitespace *>
                (primitive (\state inputStream ->
                    (state, inputStream, Ok (location.column < (currentLocation inputStream).column))
                )))
                |> andThen (\isIndented ->
                    case isIndented of
                        True -> whitespace
                        False -> spaces_
                )
            )
        )
    )

binary : OpTable -> Parser s Expression
binary ops =
  lazy <| \() ->
    let
      next =
        between_ whitespace operator |> andThen (\op ->
          choice [ Cont <$> application ops, Stop <$> expression ops ] |> andThen (\e ->
            case e of
              Cont t -> ((::) (op, t)) <$> collect
              Stop e -> succeed [(op, e)]))

      collect = next <|> succeed []
    in
      application ops |> andThen (\e ->
        collect |> andThen (\eops ->
          split ops 0 e eops))

term : OpTable -> Parser s Expression
term ops =
  lazy <| \() -> choice
    [ character
    , string
    , float
    , integer
    , access
    , variable
    , OperatorReference <$> operatorReference
    , list ops
    , record ops
    , parens (expression ops)
    , tuple ops
    , parens (many <| Combine.string ",") |> map (\i ->
        let
            x = Debug.log "i" i
        in
            String <| "createTuple" ++ (toString <| List.length i)
        )
    ]

{-| A parser for Elm expressions. -}
expression : OpTable -> Parser s Expression
expression ops =
  lazy <| \() ->
    choice [ letExpression ops
           , caseExpression ops
           , ifExpression ops
           , lambda ops
           , binary ops
           ]

op : OpTable -> String -> (Assoc, Int)
op ops n =
  Dict.get n ops
    |> Maybe.withDefault (L, 9)

assoc : OpTable -> String -> Assoc
assoc ops n = Tuple.first <| op ops n

level : OpTable -> String -> Int
level ops n = Tuple.second <| op ops n

hasLevel : OpTable -> Int -> (String, Expression) -> Bool
hasLevel ops l (n, _) = level ops n == l

split : OpTable -> Int -> Expression -> List (String, Expression) -> Parser s Expression
split ops l e eops =
  case eops of
    [] ->
      succeed e

    _ ->
      findAssoc ops l eops |> andThen (\assoc ->
        sequence (splitLevel ops l e eops) |> andThen (\es ->
          let ops_ = List.filterMap (\x -> if hasLevel ops l x
                                           then Just (Tuple.first x)
                                           else Nothing) eops
          in case assoc of
            R -> joinR es ops_
            _ -> joinL es ops_))

splitLevel : OpTable -> Int -> Expression -> List (String, Expression) -> List (Parser s Expression)
splitLevel ops l e eops =
  case break (hasLevel ops l) eops of
    (lops, (_, e_)::rops) ->
      split ops (l + 1) e lops :: splitLevel ops l e_ rops

    (lops, []) ->
      [ split ops (l + 1) e lops ]

joinL : List Expression -> List String -> Parser s Expression
joinL es ops =
  case (es, ops) of
    ([e], []) ->
      succeed e

    (a::b::remE, op::remO) ->
      joinL ((BinOp (Variable [op]) a b) :: remE) remO

    _ ->
      fail ""

joinR : List Expression -> List String -> Parser s Expression
joinR es ops =
  case (es, ops) of
    ([e], []) ->
      succeed e

    (a::b::remE, op::remO) ->
      joinR (b::remE) remO |> andThen (\e ->
        succeed (BinOp (Variable [op]) a e))

    _ ->
      fail ""

findAssoc : OpTable -> Int -> List (String, Expression) -> Parser s Assoc
findAssoc ops l eops =
  let
    lops = List.filter (hasLevel ops l) eops
    assocs = List.map (assoc ops << Tuple.first) lops
    error issue =
      let operators = List.map Tuple.first lops |> String.join " and " in
      "conflicting " ++ issue ++ " for operators " ++ operators
  in
    if List.all ((==) L) assocs then
      succeed L
    else if List.all ((==) R) assocs then
      succeed R
    else if List.all ((==) N) assocs then
      case assocs of
        [_] -> succeed N
        _   -> fail <| error "precedence"
    else
      fail <| error "associativity"


------------------------------------------------------------------------------
-- Statements
------------------------------------------------------------------------------


{-| Representations for modules' exports. -}
type ExportSet
  = AllExport
  | SubsetExport (List ExportSet)
  | FunctionExport Name
  | TypeExport Name (Maybe ExportSet)


{-| Representations for Elm's type syntax. -}
type Type
  = TypeConstructor QualifiedType (List Type)
  | TypeVariable Name
  | TypeRecordConstructor Type (List (Name, Type))
  | TypeRecord (List (Name, Type))
  | TypeTuple (List Type)
  | TypeApplication Type Type


{-| Representation for Elm's functions' parameter structure -}
type Parameter
    = RefParam String
    | AdtParam String (List Parameter)
    | TupleParam (List Parameter)
    | RecordParam (List Parameter)
--    | NamedRecordParam (List Parameter) String


{-| Function declaration type -}
type Function
    = Function Name (List Parameter) Expression


{-| Let binding type -}
type LetBinding
    = FunctionBinding Function
    | DestructuringBinding Parameter Expression


{-| Representations for Elm's statements. -}
type Statement
  = ModuleDeclaration ModuleName ExportSet
  | EffectsModuleDeclaration ModuleName Expression ExportSet
  | ImportStatement ModuleName (Maybe Alias) (Maybe ExportSet)
  | TypeAliasDeclaration Type Type
  | TypeDeclaration Type (List Type)
  | PortTypeDeclaration Name Type
  | PortDeclaration Name (List Name) Expression
  | FunctionTypeDeclaration Name Type
  | FunctionDeclaration Function
  | InfixDeclaration Assoc Int Name
  | Comment String


-- Exports
-- -------


allExport : Parser s ExportSet
allExport =
  AllExport <$ symbol ".."


functionExport : Parser s ExportSet
functionExport =
  FunctionExport <$> functionOrOperator


constructorSubsetExports : Parser s ExportSet
constructorSubsetExports =
  SubsetExport <$> commaSeparated (FunctionExport <$> upName)


constructorExports : Parser s (Maybe ExportSet)
constructorExports =
  maybe <| parens <| choice [ allExport
                            , constructorSubsetExports
                            ]


typeExport : Parser s ExportSet
typeExport =
  TypeExport <$> (upName <* spaces) <*> constructorExports


subsetExport : Parser s ExportSet
subsetExport =
  SubsetExport
    <$> commaSeparated (functionExport |> or typeExport)


exports : Parser s ExportSet
exports =
  parens <| choice [ allExport, subsetExport ]


-- Types
-- -----
typeVariable : Parser s Type
typeVariable =
  TypeVariable <$> loName

typeConstant : Parser s Type
typeConstant =
  TypeConstructor <$> sepBy1 (Combine.string ".") upName <*> succeed []

typeApplication : Parser s (Type -> Type -> Type)
typeApplication =
  TypeApplication <$ symbol "->"

typeTuple : Parser s Type
typeTuple =
  lazy <| \() ->
    TypeTuple <$> parens (commaSeparated_ type_)

typeRecordPair : Parser s (Name, Type)
typeRecordPair =
  lazy <| \() ->
    (,) <$> (loName <* symbol ":") <*> typeAnnotation

typeRecordPairs : Parser s (List (Name, Type))
typeRecordPairs =
  lazy <| \() ->
    commaSeparated_ typeRecordPair

typeRecordConstructor : Parser s Type
typeRecordConstructor =
  lazy <| \() ->
    braces
      <| TypeRecordConstructor
           <$> (between_ spaces typeVariable)
           <*> (symbol "|" *> typeRecordPairs)

typeRecord : Parser s Type
typeRecord =
  lazy <| \() ->
    braces
      <| TypeRecord <$> typeRecordPairs

typeParameter : Parser s Type
typeParameter =
  lazy <| \() ->
    between_ spaces <| choice [ typeVariable
                              , typeConstant
                              , typeRecordConstructor
                              , typeRecord
                              , typeTuple
                              , parens typeAnnotation
                              ]

typeConstructor : Parser s Type
typeConstructor =
  lazy <| \() ->
    TypeConstructor <$> sepBy1 (Combine.string ".") upName <*> many typeParameter

type_ : Parser s Type
type_ =
  lazy <| \() ->
    between_ spaces <| choice [ typeConstructor
                              , typeVariable
                              , typeRecordConstructor
                              , typeRecord
                              , typeTuple
                              , parens typeAnnotation
                              ]

typeAnnotation : Parser s Type
typeAnnotation =
  lazy <| \() ->
    type_ |> chainr typeApplication


-- Modules
-- -------
effectsModuleDeclaration : Parser s Statement
effectsModuleDeclaration =
  EffectsModuleDeclaration
    <$> (initialSymbol "effect" *> symbol "module" *> moduleName)
    <*> (symbol "where" *> record operators)
    <*> (symbol "exposing" *> exports)

moduleDeclaration : Parser s Statement
moduleDeclaration =
  ModuleDeclaration
    <$> (initialSymbol "module" *> moduleName)
    <*> (symbol "exposing" *> exports)


-- Imports
-- -------
importStatement : Parser s Statement
importStatement =
  ImportStatement
    <$> (initialSymbol "import" *> moduleName)
    <*> maybe (symbol "as" *> upName)
    <*> maybe (symbol "exposing" *> exports)


-- Type declarations
-- -----------------
typeAliasDeclaration : Parser s Statement
typeAliasDeclaration =
  TypeAliasDeclaration
    <$> (initialSymbol "type" *> symbol "alias" *> type_)
    <*> (whitespace *> symbol "=" *> typeAnnotation)

typeDeclaration : Parser s Statement
typeDeclaration =
  TypeDeclaration
    <$> (initialSymbol "type" *> type_)
    <*> (whitespace *> symbol "=" *> (sepBy1 (symbol "|") (between_ whitespace typeConstructor)))


-- Ports
-- -----


portTypeDeclaration : Parser s Statement
portTypeDeclaration =
  PortTypeDeclaration
    <$> (initialSymbol "port" *> loName)
    <*> (symbol ":" *> typeAnnotation)


portDeclaration : OpTable -> Parser s Statement
portDeclaration ops =
  PortDeclaration
    <$> (initialSymbol "port" *> loName)
    <*> (many <| between_ spaces loName)
    <*> (symbol "=" *> expression ops)


-- Functions
-- ---------


functionTypeDeclaration : Parser s Statement
functionTypeDeclaration =
  FunctionTypeDeclaration <$> (functionOrOperator <* symbol ":") <*> typeAnnotation


functionDeclaration : OpTable -> Parser s Function
functionDeclaration ops =
  Function
    <$> functionOrOperator
    <*> (many (between_ whitespace (functionParameter ops)))
    <*> (symbol "=" *> whitespace *> expression ops)


functionParameter : OpTable -> Parser s Parameter
functionParameter ops =
    lazy (\_ ->
        choice
            [ RefParam <$> loName
            , AdtParam <$> upName <*> (many (between_ whitespace (functionParameter ops)))
            , TupleParam <$> (parens <| commaSeparated (functionParameter ops))
            , RecordParam <$> (braces <| commaSeparated (RefParam <$> loName))
            --, namedRecordFields
            ]
    )


-- Infix declarations
-- ------------------


infixDeclaration : Parser s Statement
infixDeclaration =
  InfixDeclaration
    <$> choice [ L <$ initialSymbol "infixl"
               , R <$ initialSymbol "infixr"
               , N <$ initialSymbol "infix"
               ]
    <*> (spaces *> Combine.Num.int)
    <*> (spaces *> (loName <|> operator))


-- Comments
-- --------


singleLineComment : Parser s Statement
singleLineComment =
  Comment <$> (Combine.string "--" *> regex ".*" <* whitespace)


multiLineComment : Parser s Statement
multiLineComment =
  (Comment << String.fromList) <$> (Combine.string "{-" *> manyTill anyChar (Combine.string "-}"))


comment : Parser s Statement
comment =
  singleLineComment <|> multiLineComment


{-| A parser for stand-alone Elm statements. -}
statement : OpTable -> Parser s Statement
statement ops =
  choice [ effectsModuleDeclaration
         , moduleDeclaration
         , importStatement
         , typeAliasDeclaration
         , typeDeclaration
         , portTypeDeclaration
         , portDeclaration ops
         , functionTypeDeclaration
         , FunctionDeclaration <$> functionDeclaration ops
         , infixDeclaration
         , comment
         ]


{-| A parser for a series of Elm statements. -}
statements : OpTable -> Parser s (List Statement)
statements ops =
  manyTill (whitespace *> statement ops <* whitespace) end


{-| A scanner for infix statements. This is useful for performing a
first pass over a module to find all of the infix declarations in it.
-}
infixStatements : Parser s (List Statement)
infixStatements =
  let
    statements =
      many ( choice [ Just    <$> infixDeclaration
                    , Nothing <$  regex ".*"
                    ] <* whitespace ) <* end
  in
    statements |> andThen (\xs ->
      succeed <| List.filterMap identity xs)

{-| A scanner that returns an updated OpTable based on the infix
declarations in the input. -}
opTable : OpTable -> Parser s OpTable
opTable ops =
  let
    collect s d =
      case s of
        InfixDeclaration a l n ->
          Dict.insert n (a, l) d

        _ ->
          Debug.crash "impossible"
  in
    infixStatements |> andThen (\xs ->
      succeed <| List.foldr collect ops xs)


