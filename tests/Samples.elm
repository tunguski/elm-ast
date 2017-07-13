module Samples exposing (all)

import Ast exposing (parse)
import Ast.BinOp exposing (operators)
import Ast.Expression exposing (..)
import Expect exposing (..)
import String
import Test exposing (describe, test, Test)


pass : String -> Expectation
pass i =
  case parse (String.trim i) of
    Ok _ ->
      Expect.pass

    (Err (_, { position }, es)) ->
      Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


commentAfterCaseBranch = test "Comment after case branch" <| \() -> pass """
compile =
    let
        moduleName =
            case moduleDeclaration of
                Just name -> name
                _ -> [ "0" ] -- invalid identifier
        imports = 7
    in
        1
"""


applicationWithLowerIndent = test "Application with lower indent" <| \() -> pass """
typeTuple : Parser s Type
typeTuple =
    lazy <| \\() ->
        TypeTuple <$> parens (commaSeparated_
            ( choice [ typeAnnotation, type_ ] )
        )
        
reportRemBug msg c lgot rgot =
  Native.Debug.crash <|
    String.concat
    [ "Internal red-black tree invariant violated, expected "
    ]
"""


applicationWithLowerIndent2 = test "Application with lower indent 2" <| \() -> pass """
typesAliases statements env =
    statements
    |> List.map (\\(name, expr) ->
        let
            (text, newEnv) = printType env name
        in
            { newEnv
            | aliases = Dict.map
                (\\key value ->
                    moduleName ++ "." ++ value
                )
                newEnv.aliases
            }
    )
"""


letBindingWithTupleDestructuring = test "Let binding with tuple destructuring" <| \() -> pass """
complement : Color -> Color
complement color =
  case color of
    HSLA h s l a ->
      hsla (h + degrees 180) s l a

    RGBA r g b a ->
      let
        (h,s,l) = rgbToHsl r g b
      in
        hsla (h + degrees 180) s l a
"""


destructuringOfFunctionArguments = test "Destructuring of function arguments" <| \() -> pass """
never (JustOneMore nvr) =
  never nvr
"""


commentsInsideOfTypeDeclaration = test "Comments inside of type declaration" <| \() -> pass """
type NColor
    = Red
    | Black
    | BBlack  -- Double Black, counts as 2 blacks for the invariant
    | NBlack  -- Negative Black, counts as -1 blacks for the invariant

fn =
    if True {- ok! -}
    then True
    else False
"""


effectsModule = test "Effects module" <| \() -> pass """
effect module Time where { subscription = MySub } exposing (..)
"""


operatorReferencedInParentheses = test "Operator referenced in parentheses" <| \() -> pass """
sequence : List (Task x a) -> Task x (List a)
sequence tasks =
  case tasks of
    [] ->
      succeed []

    task :: remainingTasks ->
      map2 (::) task (sequence remainingTasks)
"""


recordCreation = test "Record creation" <| \() -> pass """
toHsl color =
  case color of
    HSLA h s l a ->
      { hue=h, saturation=s, lightness=l, alpha=a }

    RGBA r g b a ->
      let
        (h,s,l) = rgbToHsl r g b
      in
        { hue=h, saturation=s, lightness=l, alpha=a }
"""


destructuringInLambdaExpression = test "Destructuring in lambda expression" <| \() -> pass """
int : Int -> Int -> Generator Int
int a b =
  Generator <| \\(Seed seed) -> seed
"""


multipleDeclarationsInLetExpressions = test "Multiple declarations in let expressions" <| \() -> pass """
int : Int -> Int -> Generator Int
int a b =
  Generator <| \\(Seed seed) ->
    let
      (lo,hi) =
        if a < b then (a,b) else (b,a)
      k = hi - lo + 1
    in
      x
"""


destructureRecord = test "Destructure record" <| \() -> pass """
map : (a -> b) -> TrackedRequest a -> TrackedRequest b
map func { request, toProgress, toError } =
  { request = Http.Internal.map func request
  , toProgress = toProgress >> func
  , toError = toError >> func
  }
  
initOrUpdate msg maybeModel =
    case maybeModel of
        Uninitialized update { maybeInitialSeed, report, runs, test, init } -> init
"""


destructureFullyQualifiedAdtName = test "Destructure fully qualified ADT name" <| \() -> pass """
toTask : Request a -> Task Error a
toTask (Http.Internal.Request request) =
  Native.Http.toTask request Nothing
"""


recordUpdate = test "Record update" <| \() -> pass """
x =  { y | a = 7 }

int a b =
      ( a
      , Seed { b | state = nextState }
      )
"""


adtWithRecord = test "Adt with record" <| \() -> pass """
type Seed =
  Seed
    { state : State
    , next  : State -> (Int, State)
    , split : State -> (State, State)
    , range : State -> (Int,Int)
    }
"""


asKeyword = test "As keyword" <| \() -> pass """
balanceHelp tree =
  case tree of
    RBNode_elm_builtin Black as d -> x
    
split : State -> (State, State)
split (State s1 s2 as std) =
  let
    new_s1 =
      if s1 == magicNum6-1 then 1 else s1 + 1

    new_s2 =
      if s2 == 1 then magicNum7-1 else s2 - 1

    (State t1 t2) =
      Tuple.second (next std)
  in
    (State new_s1 t2, State t1 new_s2)
    
node =
  VirtualDom.node x

aside =
  node "aside"
"""


rest = test "Rest of cases" <| \() -> pass """
-- zero arity tuple
lazy t = (\\() -> app t)

currentLine = .line

chars = [ '\\t', '\\n' ]

position x y =
  Decode.map4
    (\\scrollLeft scrollTop offsetLeft offsetTop ->
      ( x + offsetLeft - scrollLeft, y + offsetTop - scrollTop )
    )
    scrollLeft
    scrollTop
    offsetLeft
    offsetTop
    |> andThen
      (\\( x_, y_ ) ->
        offsetParent ( x_, y_ ) (position x_ y_)
      )
      
calculatePos pos { rect, offsetWidth, offsetHeight } =
            { left = -offsetWidth
            , top = (rect.height / 2) - (offsetHeight / 2)
            }

typeAttribute = Attributes.type_

str = "\\\\\\\\"

init =
    let
        maybe : b -> (List a -> b) -> Maybe (List a) -> b
        maybe d f =
            Maybe.withDefault d << Maybe.map f
    in
        foldr (\\x -> maybe [] ((::) x) >> Just) Nothing

x : List ( TestId, () -> ( List String, List Expectation ) )

{-
    {-
    [ " top-level description failure"
    , " nested description failure"
    , " the actual test that failed"
    ]
    -}
-}

a = 7
"""


all : Test
all =
  describe "Samples suite"
    [ commentAfterCaseBranch
    , applicationWithLowerIndent
    , applicationWithLowerIndent2
    , letBindingWithTupleDestructuring
    , destructuringOfFunctionArguments
    , commentsInsideOfTypeDeclaration
    , effectsModule
    , operatorReferencedInParentheses
    , recordCreation
    , destructuringInLambdaExpression
    , multipleDeclarationsInLetExpressions
    , destructureRecord
    , destructureFullyQualifiedAdtName
    , recordUpdate
    , adtWithRecord
    , asKeyword
    , rest
    ]


