module Language exposing
    ( Expr(..)
    , ExprTag(..)
    , depth
    , parse
    , renderExpr
    , renderTag
    , texToUnicode
    , toSExpr
    )

import Dict as D
import Parser as P exposing ((|.), (|=), Parser)


type Expr
    = NodeError
    | Impossible
    | Nullary ExprTag
    | Unary ExprTag Expr
    | Binary ExprTag Expr Expr
    | LeftExprError Expr
    | BinTagError Expr
    | RightExprError ExprTag Expr Expr
    | BracketError ExprTag Expr Expr
    | LongInput Expr



varTags : List String
varTags =
    [ "p", "q", "r", "s" ]


type ExprTag
    = Var String (Maybe Int)
    | Op String


parse : String -> Result (List P.DeadEnd) Expr
parse =
    P.run
        (P.succeed (\e f -> f e)
            |= expr
            |. P.spaces
            |= P.oneOf [ P.succeed identity |. P.end, P.succeed LongInput ]
        )


variable : Parser Expr
variable =
    P.succeed
        (\str m ->
            case m of
                Just (Just x) ->
                    Nullary (Var str (Just x))

                Nothing ->
                    Nullary (Var str Nothing)

                _ ->
                    NodeError
        )
        |= P.oneOf (List.map (P.getChompedString << P.symbol) varTags)
        |= P.oneOf
            [ P.succeed Just
                |. P.symbol "_"
                |= P.oneOf
                    [ P.succeed Just |= P.int
                    , P.succeed Nothing
                    ]
            , P.succeed Nothing
            ]


unicodeTexList : List ( Int, List ( String, String ) )
unicodeTexList =
    [ ( 0, [ ( "⊤", "\\top" ), ( "⊥", "\\bot" ) ] )
    , ( 1, [ ( "¬", "\\lnot" ) ] )
    , ( 2
      , [ ( "∧", "\\land" )
        , ( "∨", "\\lor" )
        , ( "⇒", "\\Rightarrow" )
        , ( "⇔", "\\Leftrightarrow" )
        ]
      )
    ]


texToUnicode : String -> String
texToUnicode =
    unicodeTexList
        |> List.concatMap Tuple.second
        |> List.map (\( x, y ) -> String.replace y x)
        |> List.foldl (<<) identity


arity : Int -> List String
arity i =
    unicodeTexList
        |> D.fromList
        |> D.get i
        |> Maybe.withDefault []
        |> List.map Tuple.first


expr : Parser Expr
expr =
    let
        makeBinary : Expr -> Maybe ExprTag -> Expr -> Bool -> Expr
        makeBinary e1 mtag e2 b =
            case ( e1, mtag, e2 ) of
                ( NodeError, _, _ ) ->
                    LeftExprError e1

                ( _, Nothing, _ ) ->
                    BinTagError e1

                ( _, Just tag, NodeError ) ->
                    RightExprError tag e1 e2

                ( _, Just tag, _ ) ->
                    if b then
                        Binary tag e1 e2

                    else
                        BracketError tag e1 e2

        func ar x =
            P.succeed (ar <| Op x) |. P.symbol x

        -- arity 0
        --     |> List.map
    in
    [ P.oneOf (arity 0 |> List.map (func Nullary))
    , P.oneOf (arity 1 |> List.map (func Unary)) |. P.spaces |= P.lazy (\_ -> expr)
    , P.succeed makeBinary
        |. P.symbol "("
        |. P.spaces
        |= P.lazy (\_ -> expr)
        |. P.spaces
        |= P.oneOf
            [ P.oneOf (arity 2 |> List.map (func Just))
            , P.succeed Nothing |. P.chompWhile (always True)
            ]
        |. P.spaces
        |= P.lazy (\_ -> expr)
        |. P.spaces
        |= P.oneOf [ P.succeed True |. P.symbol ")", P.succeed False |. P.chompWhile (always True) ]
    , variable
    , P.succeed NodeError |. P.chompWhile (always True)
    ]
        |> P.oneOf


toSExpr : Expr -> Maybe String
toSExpr e = 
    case e of
        Nullary tag ->
            Just <| renderTag tag

        Unary tag child ->
            Just <| "(" ++ renderTag tag ++ renderExpr child ++ ")"

        Binary tag l r ->
            Just <| "(" ++ renderTag tag ++ renderExpr l ++  renderExpr r ++ ")"

        _ ->
          Nothing

renderExpr : Expr -> String
renderExpr e =
    case e of
        Nullary tag ->
            renderTag tag

        Unary tag child ->
            renderTag tag ++ renderExpr child

        Binary tag l r ->
            "(" ++ renderExpr l ++ renderTag tag ++ renderExpr r ++ ")"

        _ ->
            "Error"


renderTag : ExprTag -> String
renderTag e =
    let
        digits n =
            if n == 0 then
                []

            else
                Basics.modBy 10 n :: digits (n // 10)

        getInd n =
            digits n |> List.reverse |> List.map (\d -> Char.fromCode (Char.toCode '₀' + d)) |> String.fromList

        renderVarTag tag ind =
            tag ++ (Maybe.map getInd ind |> Maybe.withDefault "")
    in
    case e of
        Var tag ind ->
            renderVarTag tag ind

        Op tag ->
            tag


depth : Expr -> Int
depth e =
    case e of
        Nullary _ ->
            1

        Unary _ child ->
            1 + depth child

        Binary _ l r ->
            1 + max (depth l) (depth r)

        _ ->
            0
