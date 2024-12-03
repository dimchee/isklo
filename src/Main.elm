port module Main exposing (..)

-- reingold tilford '81
-- import RecExpr

import Browser
import Element as E
import Element.Input as EI
import Html exposing (Html, s)
import Html.Attributes as HA
import Html.Events as HE
import Language exposing (Expr(..))
import Table
import TreeDraw


port messageReceiver : (List String -> msg) -> Sub msg


port selectionChanged : (Selection -> msg) -> Sub msg


port changeSelection : Selection -> Cmd msg


port sendMessage : ( List Rule, String ) -> Cmd msg


type alias Rule =
    { name : String
    , searcher : String
    , applier : String
    }


type alias Selection =
    { start : Int
    , end : Int
    }


arithmeticRules : List Rule
arithmeticRules =
    [ { name = "commute-add", searcher = "(+ ?a ?b)", applier = "(+ ?b ?a)" }
    , { name = "commute-mul", searcher = "(* ?a ?b)", applier = "(* ?b ?a)" }
    , { name = "add-0", searcher = "(+ ?a 0)", applier = "?a" }
    , { name = "mul-0", searcher = "(* ?a 0)", applier = "0" }
    , { name = "mul-1", searcher = "(* ?a 1)", applier = "?a" }
    , { name = "lnot", searcher = "(¬ ⊤)", applier = "⊥" }
    , { name = "lnot", searcher = "(¬ ⊥)", applier = "⊤" }
    ]


logicRules : List Rule
logicRules =
    [ { searcher = "(∧ ⊤ ?p)", applier = "?p", name = "top_land" }
    , { searcher = "(∨ ⊤ ?p)", applier = "⊤", name = "top_lor" }
    , { searcher = "(∧ ⊥ ?p)", applier = "⊥", name = "bot_land" }
    , { searcher = "(∨ ⊥ ?p)", applier = "?p", name = "bot_lor" }
    , { searcher = "(∨ ?p (¬ ?p))", applier = "⊤", name = "p_lor_lnot_p" }
    , { searcher = "(¬ (¬ ?p))", applier = "?p", name = "lnot_lnot_p" }
    , { searcher = "(⇒ (¬ ?q) (¬ ?p))", applier = "(⇒ ?p ?q)", name = "kontrapozicija" }
    , { searcher = "(¬ (∧ ?p ?q))", applier = "(∨ (¬ ?p) (¬ ?q))", name = "demorgan_land" }
    , { searcher = "(¬ (∨ ?p ?q))", applier = "(∧ (¬ ?p) (¬ ?q))", name = "demorgan_lor" }
    , { searcher = "(⇒ ?p (⇒ ?q ?r))", applier = "(⇒ (∧ ?p ?q) ?r)", name = "curry" }
    , { searcher = "(∧ ?p ?q)", applier = "(∧ ?q ?p)", name = "commut_land" }
    , { searcher = "(∨ ?p ?q)", applier = "(∨ ?q ?p)", name = "commut_lor" }
    , { searcher = "(⇔ ?p ?q)", applier = "(⇔ ?q ?p)", name = "commut_equiv" }
    , { searcher = "(⇒ ?p ?q)", applier = "(∨ (¬ ?p) ?q)", name = "impl_def" }
    , { searcher = "(⇔ ?p ?q)", applier = "(∧ (⇒ ?p ?q) (⇒ ?q ?p))", name = "iff_def" }
    , { searcher = "(∨ ?p (∧ ?p ?q))", applier = "?p", name = "absorb_lor" }
    , { searcher = "(∧ ?p (∨ ?p ?q))", applier = "?p", name = "absorb_land" }
    , { searcher = "(∧ ?p ?p)", applier = "?p", name = "idemp_land" }
    , { searcher = "(∨ ?p ?p)", applier = "?p", name = "idemp_lor" }
    , { searcher = "(⇔ ?p ?p)", applier = "⊤", name = "p_equiv_p_top" }
    , { searcher = "(∧ ?p (∧ ?q ?r))", applier = "(∧ (∧ ?p ?q) ?r)", name = "assoc_land" }
    , { searcher = " (∨ ?p (∨ ?q ?r))", applier = "(∨ (∨ ?p ?q) ?r)", name = "assoc_lor" }
    , { searcher = " (∨ (∧ ?p ?q) (∧ ?p ?r))", applier = "(∧ ?p (∨ ?q ?r))", name = "dist_land_lor" }
    , { searcher = " (∧ (∨ ?p ?q) (∨ ?p ?r))", applier = "(∨ ?p (∧ ?q ?r))", name = "dist_lor_land" }
    ]


type alias Model =
    { draft : String
    , messages : List String
    , expr : String
    , selection : Maybe Selection
    }


type Msg
    = ExprChanged String
    | SelectionChanged { start : Int, end : Int }
    | Send String
    | Recv (List String)
    | NoOp


revRule : Rule -> Rule
revRule { searcher, applier, name } =
    { searcher = applier, applier = searcher, name = name }


main : Program () Model Msg
main =
    let
        initExpr =
            "((p ∧ q) ⇒  (q ∧ p))"

        initModel : Model
        initModel =
            { draft = "", expr = initExpr, messages = [], selection = Nothing }
    in
    Browser.element
        { init =
            always
                ( initModel
                , sendMessage
                    ( logicRules ++ List.map revRule logicRules
                      -- , "(⇔ (∧ q p) (∧ p q))"
                      -- , "(⇒ (∧ (⇒ p q) (⇒ q r)) (⇒ p r))"
                    , "(⇒  p p)"
                      --, "(⇒ p (∨ p q))"
                    )
                )
        , view = view >> E.layout []
        , update = update
        , subscriptions =
            [ selectionChanged SelectionChanged
            , messageReceiver Recv
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
                        |> Maybe.map (\{ start, end } -> { start = start + delta, end = end + delta })
            in
            ( { model | expr = str, selection = newSelection }
            , newSelection
                |> Maybe.map changeSelection
                |> Maybe.withDefault Cmd.none
            )

        SelectionChanged s ->
            ( { model | selection = Just s }, Cmd.none )

        Recv messages ->
            ( { model | messages = model.messages ++ messages }
            , Cmd.none
            )

        Send message ->
            ( model
            , sendMessage ( [], message )
            )

        NoOp ->
            ( model, Cmd.none )


viewExplanation : ( String, List String ) -> Html Msg
viewExplanation ( s, ss ) =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "justify-content" "space-between"
        , HA.style "width" "400px"
        ]
        [ Html.text s
        , Html.div [] <| List.map Html.text ss
        ]


view : Model -> E.Element Msg
view model =
    E.column [ E.padding 50, E.width E.fill, E.spacing 20 ]
        [ EI.text [ E.width E.fill, E.htmlAttribute <| HA.id "input-expr" ]
            { onChange = Language.texToUnicode >> ExprChanged
            , placeholder = Just <| EI.placeholder [] <| E.text "Expr..."
            , label = EI.labelHidden "Expression"
            , text = model.expr
            }
        , model.expr |> Language.texToUnicode |> E.text
        , E.row [ E.spacing 50, E.padding 20 ] <|
            case Language.parse model.expr of
                Ok e ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp) |> E.html |> E.el []
                    , Table.view e |> E.html |> E.el []
                    ]

                Err e ->
                    [ E.text <| Debug.toString e ]
        ]


view2 : Model -> Html Msg
view2 model =
    Html.ul []
        [ Html.input
            [ HA.type_ "text"
            , HA.placeholder "expr"
            , HE.onInput ExprChanged
            , HA.value model.expr
            ]
            []
        , Html.button [ HE.onClick <| Send model.expr ] [ Html.text "Send" ]
        , Html.div [] <|
            case Language.parse model.expr of
                Ok e ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp)
                    , Table.view e
                    ]

                Err e ->
                    [ Html.text <| Debug.toString e ]

        -- , Html.div [] <|
        --     Html.h1 [] [ Html.text "Messages: " ]
        --         :: List.map
        --             (viewExplanation
        --                 << RecExpr.print
        --                 << Maybe.withDefault (RecExpr.Tag "ERROR")
        --                 << Result.toMaybe
        --                 << RecExpr.parse
        --             )
        --             model.messages
        ]
