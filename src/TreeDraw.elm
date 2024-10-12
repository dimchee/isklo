module TreeDraw exposing (viewExpr, renderVarTag, renderTag)

import Html exposing (Html)
import Language exposing (..)
import Svg
import Svg.Attributes as SA


type Children
    = Children (List ( TreeRenderInfo, Float ))


type alias TreeRenderInfo =
    { contour :
        { left : List Float
        , right : List Float
        }
    , label : String
    , children : Children
    }


viewExpr : Expr -> Html ()
viewExpr e =
    let
        config =
            { fontSize = 20, fontScale = 0.4, nodeR = 0.15 }

        unMaybe =
            Maybe.withDefault 0

        ri =
            renderExpr config.nodeR e

        l =
            List.minimum ri.contour.left |> unMaybe

        width =
            List.maximum ri.contour.right |> unMaybe |> (\x -> x - l + config.nodeR * 2 + 0.3)

        depth x =
            List.map (Tuple.first >> depth) (getChildren x.children) |> List.maximum |> unMaybe |> (+) 1

        height =
            depth ri |> (\x -> x - 1) |> toFloat |> (+) (2 * config.nodeR + 0.1)

        getChildren (Children xs) =
            xs
    in
    Svg.svg
        [ SA.stroke "black"
        , SA.strokeWidth "0.01"
        , SA.fontSize <| String.fromFloat config.fontScale
        , SA.height <| String.fromFloat <| (config.fontSize * height / config.fontScale)
        , SA.width <| String.fromFloat <| (config.fontSize * (width / config.fontScale))
        , SA.viewBox <| String.fromFloat l ++ " " ++ String.fromFloat -config.nodeR ++ " " ++ String.fromFloat width ++ " " ++ String.fromFloat height
        ]
        [ drawTree config.nodeR 0 0 ri ]


translate : Float -> Float -> Svg.Attribute ()
translate x y =
    SA.transform <| "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


drawTree : Float -> Float -> Float -> TreeRenderInfo -> Svg.Svg ()
drawTree nodeR posx posy ri =
    let
        drawArrow x y =
            Svg.line
                [ SA.x1 "0", SA.y1 "0", SA.x2 <| String.fromFloat x, SA.y2 <| String.fromFloat y ]
                []

        drawNode label =
            [ Svg.circle [ SA.r <| String.fromFloat nodeR, SA.cx "0", SA.cy "0", SA.fill "white" ] []
            , Svg.text_ [ SA.x <| String.fromFloat nodeR, SA.y <| String.fromFloat nodeR ] [ Svg.text <| label ]
            ]

        drawChild ( x, disp ) =
            [ drawArrow disp 1
            , drawTree nodeR disp 1 x
            ]

        getChildren (Children xs) =
            xs
    in
    List.concatMap drawChild (getChildren ri.children) ++ drawNode ri.label |> Svg.g [ translate posx posy ]


renderExpr : Float -> Expr -> TreeRenderInfo
renderExpr nodeR e =
    let
        extend fst xs ys =
            fst :: (xs ++ List.drop (List.length xs) ys)

        renderInfo label children contour =
            { contour =
                { left = -nodeR :: contour.left
                , right = nodeR :: contour.right
                }
            , label = label
            , children = Children children
            }

        renderUnary str child =
            renderInfo str [ ( child, 0 ) ] child.contour

        renderBinErr str child =
            renderInfo str
                [ ( child, -1 - nodeR ) ]
                { left = List.map (\x -> x - 1 - nodeR) child.contour.left
                , right = List.map (\x -> x - 1 - nodeR) child.contour.right
                }

        renderBinary str l r =
            let
                distHalf =
                    List.map2 (-) r.contour.left l.contour.right
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> (\x -> x / -2)
                        |> Basics.max 0
                        |> (+) 1
            in
            renderInfo str
                [ ( l, -distHalf ), ( r, distHalf ) ]
                { left =
                    extend
                        -nodeR
                        (List.map (\x -> x - distHalf) l.contour.left)
                        (List.map (\x -> x + distHalf) r.contour.left)
                , right =
                    extend
                        nodeR
                        (List.map (\x -> x + distHalf) r.contour.right)
                        (List.map (\x -> x - distHalf) l.contour.right)
                }
    in
    case e of
        Nullary tag ->
            renderInfo (renderTag tag) [] { left = [], right = [] }

        Unary tag child ->
            renderUnary (renderTag tag) <| renderExpr nodeR child

        Binary tag lChild rChild ->
            renderBinary (renderTag tag) (renderExpr nodeR lChild) (renderExpr nodeR rChild)

        Impossible ->
            renderInfo "Impossible" [] { left = [], right = [] }

        NodeError ->
            renderInfo "✖" [] { left = [], right = [] }

        LeftExprError l ->
            renderBinErr "" <| renderExpr nodeR l

        BinTagError l ->
            renderBinErr "✖" <| renderExpr nodeR l

        RightExprError tag l r ->
            renderBinary (renderTag tag) (renderExpr nodeR l) (renderExpr nodeR r)

        BracketError tag l r ->
            renderBinary (renderTag tag ++ "✖") (renderExpr nodeR l) (renderExpr nodeR r)


renderVarTag : VarTag -> Maybe Int -> String
renderVarTag tag ind =
    let
        str =
            case tag of
                Language.P ->
                    "p"

                Language.Q ->
                    "q"

                Language.R ->
                    "r"

                Language.S ->
                    "s"

        digits n =
            if n == 0 then
                []

            else
                Basics.modBy 10 n :: digits (n // 10)

        getInd n =
            digits n |> List.reverse |> List.map (\d -> Char.fromCode (Char.toCode '₀' + d)) |> String.fromList
    in
    str ++ (Maybe.map getInd ind |> Maybe.withDefault "")


renderTag : ExprTag -> String
renderTag e =
    case e of
        T ->
            "⊤"

        F ->
            "⊥"

        Var tag ind ->
            renderVarTag tag ind

        Not ->
            "¬"

        And ->
            "∧"

        Or ->
            "∨"

        Impl ->
            "⇒"

        Iff ->
            "⇔"
