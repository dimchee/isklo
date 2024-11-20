module RecExpr exposing (Expr(..), parse, print)

import Parser as P exposing ((|.), (|=), Parser)


type Expr
    = Ls (List Expr)
    | Tag String


parse : String -> Result (List P.DeadEnd) Expr
parse =
    P.run expr


expr : Parser Expr
expr =
    P.oneOf
        [ P.succeed Ls
            |. P.symbol "("
            |. P.spaces
            |= P.loop []
                (\revXs ->
                    P.oneOf
                        [ P.succeed (\x loopOrDone -> loopOrDone (x :: revXs))
                            |= P.lazy (\_ -> expr)
                            |= P.oneOf
                                [ P.succeed P.Loop |. P.symbol " " |. P.spaces
                                , P.succeed (P.Done << List.reverse)
                                    |. P.spaces
                                    |. P.symbol ")"
                                ]
                        ]
                )
        , P.succeed Tag
            |= P.getChompedString
                (P.chompWhile (\c -> c /= '(' && c /= ')' && c /= ' '))
        ]

print : Expr -> String
print e = case e of
    Ls [Tag "Rewrite<=", name, x] -> print x
    Ls [Tag "Rewrite=>", name, x] -> print x
    Ls [Tag op, x, y] -> "(" ++ print x ++ op ++ print y ++ ")"
    Ls [Tag op, x] -> op ++ print x
    Tag s -> s
    _ -> "Todo"
