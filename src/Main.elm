port module Main exposing (..)

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
    div []
        [ input
            [ type_ "text"
            , placeholder "expr"
            , onInput ExprChanged
            , value model.expr
            ]
            []
        , ul []
            [ div [] [ text <| "`" ++ model.expr ++ "`" ]
            , div [] [ text <| Debug.toString <| P.run expr model.expr ]
            ]
        , P.run expr
            "(\\lnot \\bot \\lor (\\bot \\land \\lnot \\top))"
            |> Result.withDefault (Nullary T)
            |> viewExpr
        ]


type alias Pos =
    { x : Int
    , y : Int
    }


viewExpr : Expr -> Html Msg
viewExpr e =
    Svg.svg
        [ SA.stroke "black"
        , SA.height "500"
        , SA.width "500"
        ]
        [ drawExpr { x = 100, y = 10 } e ]


drawExpr : Pos -> Expr -> Svg.Svg Msg
drawExpr pos e =
    Svg.g [ translate pos ] <|
        case e of
            Nullary tag ->
                List.singleton <| drawNode { x = 0, y = 0 } tag

            Unary tag child ->
                [ drawArrow { x = 0, y = 0 } { x = 0, y = 50 }
                , drawNode { x = 0, y = 0 } tag
                , drawExpr { x = 0, y = 50 } child
                ]

            Binary tag l r ->
                [ drawArrow { x = 0, y = 0 } { x = -50, y = 50 }
                , drawArrow { x = 0, y = 0 } { x = 50, y = 50 }
                , drawExpr { x = -50, y = 50 } l
                , drawExpr { x = 50, y = 50 } r
                , drawNode { x = 0, y = 0 } tag
                ]


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


type Expr
    = Nullary ExprTag
    | Unary ExprTag Expr
    | Binary ExprTag Expr Expr


type ExprTag
    = T
    | F
    | Not
    | And
    | Or
    | Impl
    | Iff


expr : Parser Expr
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
