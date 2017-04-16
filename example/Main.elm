module Main exposing (main)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as JD
import Platform.Sub

import Combine exposing (ParseResult)


type Msg
    = Replace String
    | Loaded (Result Error String)


type alias Item = (String, Result (Combine.ParseErr ()) (Combine.ParseOk () (List Statement)))
type alias Model = List Item


files =
    [ "Array"
    , "Basics"
    , "Bitwise"
    , "Char"
    , "Color"
    , "Date"
    , "Debug"
    ]


init : (Model, Cmd Msg)
init =
    [ ( sampleModule, Ast.parse sampleModule )
    ]
    !
    (List.map (\file ->
        send Loaded (getString <| "https://raw.githubusercontent.com/elm-lang/core/5.1.1/src/" ++ file ++ ".elm")
    ) files
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
        Replace m ->
            model ! []
        Loaded result ->
            case result of
                Ok data ->
                    (( data, Ast.parse data ) :: model)
                    ! []
                Err err ->
                    let
                        x = Debug.log "error" err
                    in
                        model ! []


withChild : a -> List (Html Msg) -> Html Msg
withChild title children =
    li []
        [ pre [] [ text <| toString title ]
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
            li [] [ pre [] [ text <| toString e ] ]


statement : Statement -> Html Msg
statement s =
    case s of
        FunctionDeclaration _ _ e ->
            withChild s [ expression e ]

        s ->
            li [] [ pre [] [ text <| toString s ] ]


tree : Item -> Html Msg
tree (moduleText, ast) =
    case ast of
        ( Ok (_, _, statements)) ->
            ul [] (List.map statement statements)

        err ->
            div [] [ text <| toString err ]


view : Model -> Html Msg
view model =
    div []
        (List.map (\(txt, ast) ->
            div []
                [ textarea
                    [ on "input" (JD.map Replace targetValue)
                    , style
                        [ ( "height", "600px" )
                        , ( "width", "30%" )
                        , ( "display", "inline-block" )
                        ]
                    ]
                    [ text txt ]
                , div
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "width", "69%" )
                        , ( "vertical-align", "top" )
                        ]
                    ]
                    [ tree (txt, ast) ]
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


