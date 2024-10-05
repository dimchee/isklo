port module Main exposing (..)

-- reingold tilford '81

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Language
import TreeDraw exposing (viewExpr)


port messageReceiver : (String -> msg) -> Sub msg


port sendMessage : String -> Cmd msg


type alias Model =
    { draft : String
    , messages : List String
    , expr : String
    }


type Msg
    = ExprChanged String
    | Recv String
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init = always ( { draft = "", expr = "", messages = [] }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none -- \_ -> messageReceiver Recv
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExprChanged str ->
            ( { model | expr = str }
            , Cmd.none
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    ul []
        [ input
            [ type_ "text"
            , placeholder "expr"
            , onInput ExprChanged
            , value model.expr
            ]
            []
        , div []
            <| case Language.parse model.expr of
                Language.Parsed e -> [ viewExpr e |> Html.map (always NoOp)]
                Language.LongInput e -> [ viewExpr e |> Html.map (always NoOp), text "Long......."]
                _ -> [ text "IMPOSSIBLE" ]
        ]
