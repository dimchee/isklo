module TreeDraw exposing (viewExpr)

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


config : { fontSize : Float, fontScale : Float, spacing : Float, nodeR : Float }
config =
    { fontSize = 20, fontScale = 0.4, spacing = 0.5, nodeR = 0.15 }


viewExpr : Expr -> Html ()
viewExpr e =
    let
        unMaybe =
            Maybe.withDefault 0

        ri =
            renderExpr e

        l =
            List.minimum ri.contour.left |> unMaybe

        width =
            List.maximum ri.contour.right |> unMaybe |> (\x -> x - l)

        depth x =
            List.map (Tuple.first >> depth) (getChildren x.children) |> List.maximum |> unMaybe |> (+) 1

        height =
            depth ri |> (\x -> x - 1) |> toFloat |> (+) (2 * config.spacing)

        getChildren (Children xs) =
            xs
    in
    Svg.svg
        [ SA.stroke "black"
        , SA.strokeWidth "0.01"
        , SA.fontSize <| String.fromFloat config.fontScale
        , SA.height <| String.fromFloat <| (config.fontSize * height / config.fontScale)
        , SA.width <| String.fromFloat <| (config.fontSize * width / config.fontScale)
        , SA.viewBox <| String.fromFloat l ++ " " ++ String.fromFloat -config.spacing ++ " " ++ String.fromFloat width ++ " " ++ String.fromFloat height
        ]
        [ drawTree 0 0 ri ]


translate : Float -> Float -> Svg.Attribute ()
translate x y =
    SA.transform <| "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"


drawTree : Float -> Float -> TreeRenderInfo -> Svg.Svg ()
drawTree posx posy ri =
    let
        drawArrow x y =
            Svg.line
                [ SA.x1 "0", SA.y1 "0", SA.x2 <| String.fromFloat x, SA.y2 <| String.fromFloat y ]
                []

        drawNode label =
            [ Svg.circle [ SA.r <| String.fromFloat config.nodeR, SA.cx "0", SA.cy "0", SA.fill "white" ] []
            , Svg.text_ [ SA.x "0.2", SA.y "0.1" ] [ Svg.text <| label ]
            ]

        drawChild ( x, disp ) =
            [ drawArrow disp 1
            , drawTree disp 1 x
            ]

        getChildren (Children xs) =
            xs
    in
    List.concatMap drawChild (getChildren ri.children) ++ drawNode ri.label |> Svg.g [ translate posx posy ]


renderExpr : Expr -> TreeRenderInfo
renderExpr e =
    let
        extend fst xs ys =
            fst :: (xs ++ List.drop (List.length xs) ys)

        extendContour str contour =
            { left = -config.nodeR :: contour.left
            , right = config.nodeR + config.fontScale * (String.length str |> toFloat) :: contour.right
            }

        renderNullary str =
            { contour = extendContour str { left = [], right = [] }
            , label = str
            , children = Children []
            }

        renderUnary str child =
            { contour = extendContour str child.contour
            , label = str
            , children = Children [ ( child, 0 ) ]
            }

        renderBinErr str child =
            -- TODO 1.35?
            { contour =
                extendContour str
                    { left = List.map (\x -> x - 1.35) child.contour.left
                    , right = List.map (\x -> x - 1.35) child.contour.right
                    }
            , label = str
            , children = Children [ ( child, -1.35 ) ]
            }

        renderBinary str l r =
            let
                -- TODO problem here
                distHalf =
                    List.map2 (-) r.contour.left l.contour.right
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> (\x -> x / -2)
                        |> Basics.max 0
                        |> (+) 1
            in
            { contour =
                extendContour str
                    { left =
                        extend
                            -config.nodeR
                            (List.map (\x -> x - distHalf) l.contour.left)
                            (List.map (\x -> x + distHalf) r.contour.left)
                    , right =
                        extend
                            config.nodeR
                            (List.map (\x -> x + distHalf) r.contour.right)
                            (List.map (\x -> x - distHalf) l.contour.right)
                    }
            , label = str
            , children = Children [ ( l, -distHalf ), ( r, distHalf ) ]
            }
    in
    case e of
        Nullary tag ->
            renderNullary <| renderTag tag

        Unary tag child ->
            renderUnary (renderTag tag) <| renderExpr child

        Binary tag lChild rChild ->
            renderBinary (renderTag tag) (renderExpr lChild) (renderExpr rChild)

        Impossible ->
            renderNullary "Impossible"

        NodeError ->
            renderNullary "N"

        LeftExprError l ->
            renderBinErr "?" <| renderExpr l

        BinTagError l ->
            renderBinErr "!" <| renderExpr l

        RightExprError tag l r ->
            renderBinary (renderTag tag) (renderExpr l) (renderExpr r)

        BracketError tag l r ->
            renderBinary (renderTag tag ++ ")") (renderExpr l) (renderExpr r)


renderTag : ExprTag -> String
renderTag e =
    case e of
        T ->
            "⊤"

        F ->
            "⊥"

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
