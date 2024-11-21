module Language exposing
    ( Expr(..)
    , ExprTag(..)
    , ParsingResult(..)
    , depth
    , parse
    , renderExpr
    , renderTag
    , renderVarTag
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


type ParsingResult
    = LongInput Expr
    | Parsed Expr


varTags : List String
varTags =
    [ "p", "q", "r", "s" ]


type ExprTag
    = Var String (Maybe Int)
    | Op String


parse : String -> Result (List P.DeadEnd) ParsingResult
parse =
    P.run
        (P.succeed (\e f -> f e)
            |= expr
            |. P.spaces
            |= P.oneOf [ P.succeed Parsed |. P.end, P.succeed LongInput ]
        )


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


texToUnicode : D.Dict String String
texToUnicode =
    D.fromList
        [ ( "⊤", "\\top" )
        , ( "⊥", "\\bot" )
        , ( "¬", "\\lnot" )
        , ( "∧", "\\land" )
        , ( "∨", "\\lor" )
        , ( "⇒", "\\Rightarrow" )
        , ( "⇔", "\\Leftrightarrow" )
        ]


expr : Parser Expr
expr =
    P.oneOf
        [ P.succeed (Nullary <| Op "⊤") |. P.symbol "\\top"
        , P.succeed (Nullary <| Op "⊥") |. P.symbol "\\bot"
        , P.succeed (Unary <| Op "¬")
            |. P.symbol "\\lnot"
            |. P.spaces
            |= P.lazy (\_ -> expr)
        , P.succeed makeBinary
            |. P.symbol "("
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |= P.oneOf
                [ P.succeed (Just <| Op "∧") |. P.symbol "\\land"
                , P.succeed (Just <| Op "∨") |. P.symbol "\\lor"
                , P.succeed (Just <| Op "⇒") |. P.symbol "\\Rightarrow"
                , P.succeed (Just <| Op "⇔") |. P.symbol "\\Leftrightarrow"
                , P.succeed Nothing |. P.chompWhile (always True)
                ]
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |= P.oneOf [ P.succeed True |. P.symbol ")", P.succeed False |. P.chompWhile (always True) ]
        , variable
        , P.succeed NodeError |. P.chompWhile (always True)
        ]


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


renderVarTag : String -> Maybe Int -> String
renderVarTag tag ind =
    let
        digits n =
            if n == 0 then
                []

            else
                Basics.modBy 10 n :: digits (n // 10)

        getInd n =
            digits n |> List.reverse |> List.map (\d -> Char.fromCode (Char.toCode '₀' + d)) |> String.fromList
    in
    tag ++ (Maybe.map getInd ind |> Maybe.withDefault "")


renderTag : ExprTag -> String
renderTag e =
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
