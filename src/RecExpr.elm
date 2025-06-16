module RecExpr exposing (Expr(..), parse, toRewrite)

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


type alias Rewrite =
    { expr : String
    , rules : List String
    }


toRewrite : Expr -> Maybe Rewrite
toRewrite =
    print >> Maybe.map (\( e, rs ) -> { expr = e, rules = rs })


print : Expr -> Maybe ( String, List String )
print e =
    case e of
        Ls [ Tag "Rewrite<=", Tag name, x ] ->
            print x |> Maybe.map (Tuple.mapSecond ((::) name))

        Ls [ Tag "Rewrite=>", Tag name, x ] ->
            print x |> Maybe.map (Tuple.mapSecond ((::) name))

        Ls [ Tag op, x, y ] ->
            Maybe.map2 (\(px, xs) (py, ys) ->
            ( "(" ++ px ++ op ++ py ++ ")", xs ++ ys ))
                (print x) (print y)

        Ls [ Tag op, x ] ->
            print x |> Maybe.map (Tuple.mapFirst (\a -> op ++ a))

        Tag s ->
            Just ( s, [] )

        _ ->
            Nothing
