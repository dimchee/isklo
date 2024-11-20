module Table exposing (view)

import Html exposing (Html)
import Html.Attributes as HA
import Language
import List.Extra


type alias VarData =
    { tag : Language.VarTag, index : Maybe Int }


type alias Valuation =
    List ( VarData, Bool )


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


view : Language.Expr -> Html msg
view expr =
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
