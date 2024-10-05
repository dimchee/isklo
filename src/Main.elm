port module Main exposing (..)

-- reingold tilford 81

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser as P exposing ((|.), (|=), Parser)
import Svg
import Svg.Attributes as SA


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


view : Model -> Html Msg
view model =
    let
        fakeExpr =
            P.run expr "((\\lnot \\top \\land ((\\bot \\land \\top) \\lor (\\top \\lor \\top)))  \\lor (\\lnot \\bot \\land \\lnot \\top))" |> Result.withDefault (Nullary T)
    in
    div []
        [ input
            [ type_ "text"
            , placeholder "expr"
            , onInput ExprChanged
            , value model.expr
            ]
            []
        , ul []
            -- [ div [] [ text <| "`" ++ model.expr ++ "`" ]
            -- , div [] [ text <| Debug.toString <| P.run expr model.expr ]
            [ div [] [ text <| Debug.toString <| renderExpr fakeExpr ]
            ]
        , viewExpr fakeExpr
        ]


type alias Pos =
    { x : Int
    , y : Int
    }


type alias RenderInfo =
    { contour :
        { left : List Float
        , right : List Float
        }
    , tag : ExprTag
    }


viewExpr : Expr ExprTag -> Html Msg
viewExpr e =
    let
        ri =
            renderExpr e
    in
    Svg.svg
        [ SA.stroke "black"
        , SA.height "500"
        , SA.width "500"
        ]
        [ drawExpr { x = 200, y = 10 } ri ]


drawExpr : Pos -> Expr RenderInfo -> Svg.Svg Msg
drawExpr pos e =
    Svg.g [ translate pos ] <|
        case e of
            Nullary ri ->
                List.singleton <| drawNode { x = 0, y = 0 } ri.tag

            Unary ri child ->
                [ drawArrow { x = 0, y = 0 } { x = 0, y = 50 }
                , drawNode { x = 0, y = 0 } ri.tag
                , drawExpr { x = 0, y = 50 } child
                ]

            Binary ri l r ->
                let
                    disp =
                        (List.drop 1 ri.contour.left |> List.head |> Maybe.withDefault 0) * 50 |> round
                in
                [ drawArrow { x = 0, y = 0 } { x = -disp, y = 50 }
                , drawArrow { x = 0, y = 0 } { x = disp, y = 50 }
                , drawExpr { x = disp, y = 50 } l
                , drawExpr { x = -disp, y = 50 } r
                , drawNode { x = 0, y = 0 } ri.tag
                ]


renderExpr : Expr ExprTag -> Expr RenderInfo
renderExpr e =
    case e of
        Nullary tag ->
            Nullary
                { contour = { left = [ 0 ], right = [ 0 ] }
                , tag = tag
                }

        Unary tag child ->
            let
                c =
                    renderExpr child
            in
            Unary
                { contour =
                    { left = 0 :: (getTag c).contour.left
                    , right = 0 :: (getTag c).contour.right
                    }
                , tag = tag
                }
                c

        Binary tag lChild rChild ->
            let
                l =
                    renderExpr lChild

                r =
                    renderExpr rChild

                distHalf =
                    List.map2 (-) (getTag r).contour.left (getTag l).contour.right
                        |> List.minimum
                        |> Maybe.withDefault 0
                        |> (\x -> x / -2)
                        |> Basics.max 0
                        |> (+) 1
            in
            Binary
                { contour =
                    { left = 0 :: List.map (\x -> x - distHalf) (getTag l).contour.left
                    , right = 0 :: List.map (\x -> x + distHalf) (getTag r).contour.right
                    }
                , tag = tag
                }
                l
                r

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


translate : Pos -> Svg.Attribute Msg
translate pos =
    SA.transform <| "translate(" ++ String.fromInt pos.x ++ "," ++ String.fromInt pos.y ++ ")"


drawArrow : Pos -> Pos -> Svg.Svg Msg
drawArrow pos1 pos2 =
    Svg.line
        [ SA.x1 <| String.fromInt pos1.x
        , SA.y1 <| String.fromInt pos1.y
        , SA.x2 <| String.fromInt pos2.x
        , SA.y2 <| String.fromInt pos2.y
        ]
        []


drawNode : Pos -> ExprTag -> Svg.Svg Msg
drawNode pos x =
    Svg.g [ translate pos ]
        [ Svg.circle [ SA.r "5", SA.cx "0", SA.cy "0", SA.fill "white" ] []
        , Svg.text_ [ SA.x "10", SA.y "5" ] [ Svg.text <| renderTag x ]
        ]


type Expr a
    = Nullary a
    | Unary a (Expr a)
    | Binary a (Expr a) (Expr a)


getTag : Expr a -> a
getTag e =
    case e of
        Nullary tag ->
            tag

        Unary tag _ ->
            tag

        Binary tag _ _ ->
            tag


type ExprTag
    = T
    | F
    | Not
    | And
    | Or
    | Impl
    | Iff


expr : Parser (Expr ExprTag)
expr =
    P.oneOf
        [ P.succeed (Nullary T) |. P.symbol "\\top"
        , P.succeed (Nullary F) |. P.symbol "\\bot"
        , P.succeed (Unary Not)
            |. P.symbol "\\lnot"
            |. P.spaces
            |= P.lazy (\_ -> expr)
        , P.succeed (\l tag r -> Binary tag l r)
            |. P.symbol "("
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |= P.oneOf
                [ P.succeed And |. P.symbol "\\land"
                , P.succeed Or |. P.symbol "\\lor"
                , P.succeed Impl |. P.symbol "\\Rightarrow"
                , P.succeed Iff |. P.symbol "\\Leftrightarrow"
                ]
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |. P.symbol ")"
        ]


