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
    = Replace String String String
    | Loaded String String (Result Error String)


type alias Item =
    { moduleName : String
    , text : Maybe String
    , parsed : Maybe (Result
                (Combine.ParseErr ())
                (Combine.ParseOk () (List Statement)))
    }
type alias Package =
    { package : String
    , items : List Item
    }
type alias Model = List Package


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
    , ("Bogdanp/elm-combine/3.1.1/src/",
        [ "Combine"
        , "Combine/Char"
        ])
    , ("elm-lang/html/2.0.0/src/",
        [ "Html"
        , "Html/Attributes"
        ])
    ]


init : (Model, Cmd Msg)
init =
    (Package "n/a" [ Item "Custom Editor"
        (Just sampleModule)
        (Just <| Ast.parse sampleModule) ]
    ::
    List.map (\(pkg, items) ->
        Package pkg <|
            List.map (\moduleName ->
                Item moduleName Nothing Nothing
            ) items
    ) testFiles
    )
    !
    ((List.concatMap (\(package, files) ->
        List.map (\file ->
            send (Loaded package file)
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
        Replace pkg name m ->
            List.map (\package ->
                if package.package == pkg then
                    { package | items = List.map (\item ->
                        if item.moduleName == name then
                            { item
                            | text = Just m
                            , parsed = Just (Ast.parse m)
                            }
                        else
                            item
                    ) package.items }
                else
                    package
            ) model
            ! []
        Loaded pkg name result ->
            case result of
                Ok data ->
                    List.map (\package ->
                        if package.package == pkg then
                            { package | items = List.map (\item ->
                                if item.moduleName == name then
                                    { item
                                    | text = Just data
                                    , parsed = Just (Ast.parse data)
                                    }
                                else
                                    item
                            ) package.items }
                        else
                            package
                    ) model
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


--tree : Item -> Html Msg
tree ast =
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


countItems : Bool -> Model -> Int
countItems value model =
    model
    |> List.map .items
    |> List.concatMap identity
    |> List.filter (\i ->
        case i.parsed of
            Just ast ->
                case ast of
                    Ok _ -> value
                    _ -> not value
            _ -> False
    )
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
                , text <| toString (
                    model
                    |> List.map .items
                    |> List.concatMap identity
                    |> List.length
                    )
                , text " Success count: "
                , text <| toString <| countItems True model
                , text " Failure count: "
                , text <| toString <| countItems False model
                ]
            ]
        ]
    ]
    ++
    (List.concatMap identity
        (List.map (\package ->
            [ Grid.simpleRow
                [ Grid.col [ Col.xs12 ]
                    [ h2 [] [ text <| "Package: " ++ package.package ] ]
                ]
            ]
            ++
            (List.concatMap identity
                (List.map (\item ->
                    [ Grid.simpleRow
                        [ Grid.col [ Col.xs12 ]
                            [ h2 [] [ text <| "Module: " ++ item.moduleName ] ]
                        ]
                    , Grid.simpleRow
                        [ Grid.col
                            [ Col.xs4 ]
                            [ textarea
                                [ on "input" (JD.map (Replace package.package item.moduleName) targetValue)
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
                                [ text <| case item.text of
                                    Just txt -> txt
                                    _ -> ""
                                ]
                            ]
                        , Grid.col
                            ((Col.attrs <| List.singleton <| class <|
                                case item.parsed of
                                    Just ast ->
                                        case ast of
                                            Ok _ -> "card-success"
                                            _ -> "card-danger"
                                    _ ->
                                        ""
                            )
                            ::
                            [ Col.xs8
                            ])
                            [ case item.parsed of
                                Just ast -> tree ast
                                _ -> text ""
                            ]
                        ]
                    ]
                ) package.items))
        ) model))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , view = view
        }


