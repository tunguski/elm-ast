module Main exposing (main)

import Ast
import Ast.Expression exposing (..)
import Ast.Helpers
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Platform.Sub

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col

import Combine exposing (ParseResult)


type Msg
    = Replace String String
    | Loaded String (Result Error String)


type alias Item = (String, String, Result (Combine.ParseErr ()) (Combine.ParseOk () (List Statement)))
type alias Model = List Item


testFiles =
    [ ("elm-lang/core/5.1.1/src/",
        [ "Array"
        , "Basics"
        , "Bitwise"
        , "Char"
        , "Color"
        , "Date"
        , "Debug"
        , "Dict"
        , "Json/Decode"
        , "Json/Encode"
        , "List"
        , "Maybe"
        , "Platform"
        , "Platform/Cmd"
        , "Platform/Sub"
        , "Process"
        , "Random"
        , "Regex"
        , "Result"
        , "Set"
        , "String"
        , "Task"
        , "Time"
        , "Tuple"
        ])
    , ("elm-lang/http/1.0.0/src/",
        [ "Http"
        , "Http/Progress"
        ])
    ]


init : (Model, Cmd Msg)
init =
    [ ( "custom", sampleModule, Ast.parse sampleModule )
    ]
    !
    ((List.concatMap (\(package, files) ->
        List.map (\file ->
            send (Loaded (package ++ " - " ++ file))
                (getString <| "https://raw.githubusercontent.com/" ++ package ++ file ++ ".elm")
        ) files
    )) testFiles
    )


sampleModule =
    """module Main exposing (..)

f : Int -> Int
f x = x + 1

g : Int -> Int
g x = x * 2

h = f << g


update : Msg -> String -> String
update action model =
    case action of
        Replace m ->
            m


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ pre [] [ text <| toString title ]
        , ul [] children
        ]
"""


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Replace name m ->
            List.map (\(elementName, str, ast) ->
                case elementName == name of
                    True -> (elementName, m, Ast.parse m)
                    False -> (elementName, str, ast)
            ) model ! []
        Loaded name result ->
            case result of
                Ok data ->
                    (( name, data, Ast.parse data ) :: model)
                    ! []
                Err err ->
                    let
                        x = Debug.log "error" err
                    in
                        model ! []


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ text <| toString title
        , ul [] children
        ]


expression : Expression -> Html Msg
expression e =
    case e of
        List es ->
            withChild e (List.map expression es)

        Application e1 e2 ->
            withChild e
                [ expression e1
                , expression e2
                ]

        e ->
            li [] [ text <| toString e ]


statement : Statement -> Html Msg
statement s =
    case s of
        FunctionDeclaration (Function _ _ e) ->
            withChild s [ expression e ]

        s ->
            li [] [ text <| toString s ]


tree : Item -> Html Msg
tree (name, moduleText, ast) =
    case ast of
        ( Ok (_, _, statements)) ->
            ul [] (List.map statement statements)

        err ->
            div
                [ style
                    [ ( "margin", "10px" )
                    , ( "min-height", "600px" )
                    ]
                ]
                [ text <| toString err ]


countItems value model =
    List.filter (\i ->
        case i of
            (_, _, Ok _) -> value
            _ -> not value
    ) model
    |> List.length


view : Model -> Html Msg
view model =
    Grid.containerFluid [] <|
        [ CDN.stylesheet
        --, navbar model
        ]
        ++
        mainContent model


mainContent model =
    [ Grid.simpleRow
        [ Grid.col
            [ Col.xs12 ]
            [ h1 []
                [ text "All items: "
                , text <| toString <| List.length model
                , text " Success count: "
                , text <| toString <| countItems True model
                , text " Failure count: "
                , text <| toString <| countItems False model
                ]
            ]
        ]
    ]
    ++
    (List.map (\(name, txt, ast) ->
        Grid.simpleRow
            [ Grid.col
                [ Col.xs4 ]
                [ textarea
                    [ on "input" (JD.map (Replace name) targetValue)
                    , style
                        [ ( "width", "100%" )
                        , ( "padding", "0" )
                        , ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "bottom", "0" )
                        , ( "left", "0" )
                        , ( "right", "0" )
                        ]
                    ]
                    [ text txt ]
                ]
            , Grid.col
                ((Col.attrs <| List.singleton <| class <|
                    case ast of
                        Ok _ -> "card-success"
                        _ -> "card-danger"
                )
                ::
                [ Col.xs8
                ])
                [ tree (name, txt, ast) ]
            ]
    ) model)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , view = view
        }


