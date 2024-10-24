module Language exposing
    ( Expr(..)
    , ExprTag(..)
    , ParsingResult(..)
    , VarTag(..)
    , parse
    , renderExpr
    , renderTag
    , renderVarTag, depth
    )

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


renderVarTag : VarTag -> Maybe Int -> String
renderVarTag tag ind =
    let
        str =
            case tag of
                P ->
                    "p"

                Q ->
                    "q"

                R ->
                    "r"

                S ->
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


depth : Expr -> Int
depth e = case e of
    Nullary _ -> 1
    Unary _ child -> 1 +depth child
    Binary _ l r -> 1 + max (depth l) (depth r)
    _ -> 0

