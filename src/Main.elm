port module Main exposing (..)

-- reingold tilford '81
-- import RecExpr

import Browser
import Element as E
import Element.Border as Border
import Element.Input as EI
import Html
import Html.Attributes as HA
import Language exposing (Expr(..))
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
    { draft : String
    , explanation : List String
    , expr : String
    , selection : Selection
    }


type Msg
    = ExprChanged String
    | SelectionChanged { start : Int, end : Int }
    | Explain 
    | GotExplanation (List String)
    | NoOp


reqExp : String -> Cmd msg
reqExp expr =
    requestExplanation
        ( Rule.logicRules ++ List.map Rule.revRule Rule.logicRules
          -- , "(⇔ (∧ q p) (∧ p q))"
          -- , "(⇒ (∧ (⇒ p q) (⇒ q r)) (⇒ p r))"
        , expr
          --, "(⇒ p (∨ p q))"
        )


main : Program () Model Msg
main =
    let
        initExpr =
            "((p ∧ q) ⇒  (q ∧ p))"

        initModel : Model
        initModel =
            { draft = "", expr = initExpr, explanation = [], selection = { start = 0, end = 0 } }
    in
    Browser.element
        { init = always ( initModel, Cmd.none )
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
                    String.length str - String.length model.expr

                newSelection =
                    model.selection
                        |> (\{ start, end } ->
                                if start == end then
                                    { start = start + delta, end = end + delta }

                                else
                                    { start = start, end = start }
                           )
            in
            ( { model | expr = str, selection = newSelection }
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
            , Language.parse model.expr
                |> Result.toMaybe 
                |> Maybe.andThen Language.toSExpr
                |> Maybe.map reqExp
                |> Maybe.withDefault Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


viewExplanation : ( String, List String ) -> E.Element Msg
viewExplanation ( s, ss ) =
    E.text s
        :: List.map E.text ss
        |> E.column []



-- Html.div
--     [ HA.style "display" "flex"
--     , HA.style "flex-direction" "row"
--     , HA.style "justify-content" "space-between"
--     , HA.style "width" "400px"
--     ]
--     [ Html.text s
--     , Html.div [] <| List.map Html.text ss
--     ]


view : Model -> E.Element Msg
view model =
    E.column [ E.padding 50, E.width E.fill, E.spacing 20 ]
        [ E.row [ E.width E.fill, E.padding 20, E.spacing 20 ]
            [ EI.text [ E.width E.fill, E.htmlAttribute <| HA.id "input-expr" ]
                { onChange = Language.texToUnicode >> ExprChanged
                , placeholder = Just <| EI.placeholder [] <| E.text "Expr..."
                , label = EI.labelHidden "Expression"
                , text = model.expr
                }
            , EI.button [ Border.width 2, E.padding 10 ]
                { onPress = Just Explain , label = E.text "explain" }
            ]
        , model.expr |> Language.texToUnicode |> E.text
        , E.row [ E.spacing 50, E.padding 20 ] <|
            case Language.parse model.expr of
                Ok e ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp) |> E.html |> E.el []
                    , Table.view e |> E.html |> E.el []
                    ]

                Err e ->
                    [ E.text <| Debug.toString e ]
        , E.row [] <| [ viewExplanation ( model.expr, model.explanation ) ]
        ]
