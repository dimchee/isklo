port module Main exposing (..)

-- reingold tilford '81

import Browser
import Element as E
import Element.Border as Border
import Element.Input as EI
import Html
import Html.Attributes as HA
import Language exposing (Expr(..))
import RecExpr
import Rule exposing (Rule)
import Table
import TreeDraw


port requestExplanation : ( List Rule, String ) -> Cmd msg


port gotExplanation : (List String -> msg) -> Sub msg


port changeSelection : Selection -> Cmd msg


port selectionChanged : (Selection -> msg) -> Sub msg


type alias Selection =
    { start : Int
    , end : Int
    }


type alias Model =
    { explanation : List String
    , inputString : String
    , selection : Selection
    }


type Msg
    = ExprChanged String
    | SelectionChanged { start : Int, end : Int }
    | Explain
    | GotExplanation (List String)
    | NoOp


main : Program () Model Msg
main =
    Browser.element
        { init =
            always <|
                update Explain <|
                    { inputString = "((p ⇒ q) ⇔(¬ q ⇒ ¬p))"
                    , explanation = []
                    , selection = { start = 0, end = 0 }
                    }
        , view = view >> E.layout []
        , update = update
        , subscriptions =
            [ selectionChanged SelectionChanged
            , gotExplanation GotExplanation
            ]
                |> Sub.batch
                |> always
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExprChanged str ->
            let
                delta =
                    String.length str - String.length model.inputString

                newSelection =
                    model.selection
                        |> (\{ start, end } ->
                                if start == end then
                                    { start = start + delta, end = end + delta }

                                else
                                    { start = start, end = start }
                           )
            in
            ( { model | inputString = str, selection = newSelection }
            , newSelection
                |> changeSelection
            )

        SelectionChanged s ->
            ( { model | selection = s }, Cmd.none )

        GotExplanation explanation ->
            ( { model | explanation = explanation }
            , Cmd.none
            )

        Explain ->
            ( model
            , Language.parse model.inputString
                |> Language.toSExpr
                |> Maybe.map
                    (\expr -> requestExplanation ( Rule.logicRules, expr ))
                |> Maybe.withDefault Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


viewExplanation : List String -> E.Element Msg
viewExplanation ss =
    let
        getRewrite =
            RecExpr.parse
                >> Result.toMaybe
                >> Maybe.andThen RecExpr.toRewrite
    in
    E.table [ E.spacingXY 20 5, E.padding 20 ]
        { data = List.filterMap getRewrite ss
        , columns =
            [ { header = E.text "Expression"
              , width = E.shrink
              , view = \{ expr } -> E.text expr
              }
            , { header = E.text "Rule"
              , width = E.shrink
              , view = \{ rules } -> List.map E.text rules |> E.row []
              }
            ]
        }


view : Model -> E.Element Msg
view model =
    let
        expr =
            Language.parse model.inputString
    in
    E.column [ E.padding 50, E.width E.fill, E.spacing 20 ]
        [ E.row [ E.width E.fill, E.padding 20, E.spacing 20 ]
            [ EI.text [ E.width E.fill, E.htmlAttribute <| HA.id "input-expr" ]
                { onChange = Language.texToUnicode >> ExprChanged
                , placeholder = Just <| EI.placeholder [] <| E.text "Expr..."
                , label = EI.labelHidden "Expression"
                , text = model.inputString
                }
            , EI.button [ Border.width 2, E.padding 10 ]
                { onPress = Just Explain, label = E.text "explain" }
            ]
        , TreeDraw.viewExpr expr |> Html.map (always NoOp) |> E.html |> E.el []
        , Table.view expr |> E.html |> E.el []
        , viewExplanation model.explanation
        ]
