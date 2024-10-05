module Language exposing (Expr(..), ExprTag(..), ParsingResult(..), parse)

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

type ParsingResult = UnknownError String | LongInput Expr | Parsed Expr


type ExprTag
    = T
    | F
    | Not
    | And
    | Or
    | Impl
    | Iff


parse : String -> ParsingResult
parse s =
    case P.run (P.succeed (\e f -> f e) 
        |= expr 
        |. P.spaces
        |= P.oneOf [ P.succeed Parsed |. P.end, P.succeed LongInput ]
    ) s of
        Ok x ->
            x

        Err e ->
            UnknownError <| Debug.toString e


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
        , P.succeed NodeError |. P.chompWhile (always True)
        ]
