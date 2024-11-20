port module Main exposing (..)

-- reingold tilford '81

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Language exposing (Expr(..))
import Table
import TreeDraw


port messageReceiver : (String -> msg) -> Sub msg


port sendMessage : ( List Rule, String ) -> Cmd msg


type alias Rule =
    { name : String
    , searcher : String
    , applier : String
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
    , { searcher = " (∧ (∨ ?p ?q) (∨ ?p ?r))", applier = "(∨?p (∧ ?q ?r))", name = "dist_lor_land" }
    ]

type alias Model =
    { draft : String
    , messages : List String
    , expr : String
    }


type Msg
    = ExprChanged String
    | Send String
    | Recv String
    | NoOp


main : Program () Model Msg
main =
    let
        initExpr =
            "((p \\land q) \\Rightarrow (q \\land p))"

        initModel =
            { draft = "", expr = initExpr, messages = [] }

    in
    Browser.element
        { init = always ( initModel, sendMessage ( logicRules, "(⇔ (∧ q p) (∧ p q))" ) )
        , view = view
        , update = update
        , subscriptions = \_ -> messageReceiver Recv
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

        Send message ->
            ( model
            , sendMessage ( [], message )
            )

        NoOp ->
            ( model, Cmd.none )


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
        , Html.button [ HE.onClick <| Send model.expr ] [ Html.text "Send" ]
        , Html.div [] <|
            case Language.parse model.expr of
                Ok (Language.Parsed e) ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp)
                    , Table.view e
                    ]

                Ok (Language.LongInput e) ->
                    [ TreeDraw.viewExpr e |> Html.map (always NoOp), Html.text "Long......." ]

                Err e ->
                    [ Html.text <| Debug.toString e ]
        , Html.div [] <|
            Html.h1 [] [ Html.text "Messages: " ]
                :: List.map
                    (Html.div []
                        << List.singleton
                        << Html.text
                    )
                    model.messages
        ]
