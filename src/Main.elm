port module Main exposing (..)

-- reingold tilford '81

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Language exposing (Expr(..))
import List.Extra
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
        { init = always ( { draft = "", expr = "((p \\land q) \\Rightarrow (q \\land p))", messages = [] }, Cmd.none )
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


type alias VarData =
    { tag : Language.VarTag, index : Maybe Int }


type alias Valuation =
    List ( VarData, Bool )


vars : Language.Expr -> List VarData
vars e =
    case e of
        Language.Nullary t ->
            case t of
                Language.Var tag index ->
                    [ { tag = tag, index = index } ]

                _ ->
                    []

        Language.Unary _ child ->
            vars child

        Language.Binary _ l r ->
            List.Extra.unique <| vars l ++ vars r

        _ ->
            []


subExprs : Language.Expr -> List Language.Expr
subExprs e =
    case e of
        Language.Nullary _ ->
            [ e ]

        Language.Unary _ child ->
            e :: subExprs child

        Language.Binary _ l r ->
            e :: subExprs l ++ subExprs r

        _ ->
            []


interpret : Valuation -> Language.Expr -> Maybe Bool
interpret vs e =
    case e of
        Language.Nullary Language.T ->
            Just True

        Language.Nullary Language.F ->
            Just False

        Language.Nullary (Language.Var tag index) ->
            List.Extra.find (\( var, _ ) -> var.tag == tag && var.index == index) vs
                |> Maybe.map Tuple.second

        Language.Unary Language.Not child ->
            interpret vs child |> Maybe.map not

        Language.Binary Language.And l r ->
            Maybe.map2 (&&) (interpret vs l) (interpret vs r)

        Language.Binary Language.Or l r ->
            Maybe.map2 (||) (interpret vs l) (interpret vs r)

        Language.Binary Language.Impl l r ->
            Maybe.map2 (||) (interpret vs l) (Maybe.map not <| interpret vs r)

        Language.Binary Language.Iff l r ->
            Maybe.map2 (==) (interpret vs l) (interpret vs r)

        _ ->
            Nothing


viewTable : Language.Expr -> Html Msg
viewTable expr =
    let
        valuations =
            vars expr
                |> List.map (\var -> [ ( var, True ), ( var, False ) ])
                |> List.Extra.cartesianProduct

        sExprs =
            subExprs expr |> List.Extra.unique |> List.sortBy Language.depth

        viewTruth mx =
            case mx of
                Just True ->
                    "⊤"

                Just False ->
                    "⊥"

                Nothing ->
                    "✖"

        style =
            [ HA.style "border" "1px solid black"
            , HA.style "border-collapse" "collapse"
            , HA.style "text-align" "center"
            , HA.style "padding" "5px"
            ]

        viewValuation val =
            sExprs
                |> List.map (interpret val >> viewTruth >> Html.text >> List.singleton >> Html.td style)
                |> Html.tr style
    in
    sExprs
        |> List.map (Language.renderExpr >> Html.text >> List.singleton >> Html.th style)
        |> Html.tr []
        |> (\l -> l :: List.map viewValuation valuations)
        |> Html.table style


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
