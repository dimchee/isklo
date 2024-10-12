port module Main exposing (..)

-- reingold tilford '81

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Language
import TreeDraw


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


viewTable : Language.Expr -> Html Msg
viewTable expr =
    let
        vars =
            Language.vars expr

        allCombs l =
            case l of
                _ :: xs ->
                    List.concatMap (\ys -> [ Language.T :: ys, Language.F :: ys ]) <| allCombs xs

                [] ->
                    [ [] ]

        viewValues =
            allCombs vars |> List.map (Html.tr [] << List.map (\x -> Html.th [] [ Html.text <| TreeDraw.renderTag x ]))
    in
    Html.table [] <|
        (vars
            |> List.map (\v -> Html.th [] [ Html.text <| TreeDraw.renderVarTag v.tag v.index ])
            |> Html.tr []
        )
            :: viewValues


view : Model -> Html Msg
view model =
    Html.ul []
        [ Html.input
            [ HA.type_ "text"
            , HA.placeholder "expr"
            , HE.onInput ExprChanged
            , HA.value model.expr
            ]
            []
        , Html.div [] <|
            case Language.parse model.expr of
                Ok (Language.Parsed e) ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp)
                    , viewTable e
                    ]

                Ok (Language.LongInput e) ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp), Html.text "Long......." ]

                Err e ->
                    [ Html.text <| Debug.toString e ]
        ]
