module Language exposing (Expr(..), ExprTag(..), ParsingResult(..), VarTag(..), parse)

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


type VarTag
    = P
    | Q
    | R
    | S


type ExprTag
    = Var VarTag (Maybe Int)
    | T
    | F
    | Not
    | And
    | Or
    | Impl
    | Iff


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
        |= P.oneOf
            [ P.succeed P |. P.symbol "p"
            , P.succeed Q |. P.symbol "q"
            , P.succeed R |. P.symbol "r"
            , P.succeed S |. P.symbol "s"
            ]
        |= P.oneOf
            [ P.succeed Just
                |. P.symbol "_"
                |= P.oneOf
                    [ P.succeed Just |= P.int
                    , P.succeed Nothing
                    ]
            , P.succeed Nothing
            ]


expr : Parser Expr
expr =
    P.oneOf
        [ P.succeed (Nullary T) |. P.symbol "\\top"
        , P.succeed (Nullary F) |. P.symbol "\\bot"
        , P.succeed (Unary Not)
            |. P.symbol "\\lnot"
            |. P.spaces
            |= P.lazy (\_ -> expr)
        , P.succeed makeBinary
            |. P.symbol "("
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |= P.oneOf
                [ P.succeed (Just And) |. P.symbol "\\land"
                , P.succeed (Just Or) |. P.symbol "\\lor"
                , P.succeed (Just Impl) |. P.symbol "\\Rightarrow"
                , P.succeed (Just Iff) |. P.symbol "\\Leftrightarrow"
                , P.succeed Nothing |. P.chompWhile (always True)
                ]
            |. P.spaces
            |= P.lazy (\_ -> expr)
            |. P.spaces
            |= P.oneOf [ P.succeed True |. P.symbol ")", P.succeed False |. P.chompWhile (always True) ]
        , variable
        , P.succeed NodeError |. P.chompWhile (always True)
        ]
