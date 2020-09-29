module Math exposing
    ( Expr
    , evaluate
    , parse
    )

import Parser exposing (..)


type Expr
    = Value Int
    | Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr


evaluate : Expr -> Int
evaluate expr =
    case expr of
        Value n ->
            n

        Add a b ->
            evaluate a + evaluate b

        Subtract a b ->
            evaluate a - evaluate b

        Multiply a b ->
            evaluate a * evaluate b


parse : String -> Result (List DeadEnd) Expr
parse string =
    run expression string


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]



-- PARSER


{-| A term is a standalone chunk of math, like `4` or `(3 + 4)`. We use it as
a building block in larger expressions.
-}
term : Parser Expr
term =
    succeed Value
        |= myInt


{-| Every expression starts with a term. After that, it may be done, or there
may be a `+` or `*` sign and more math.
-}
expression : Parser Expr
expression =
    term
        |> andThen (expressionHelp [])


{-| Once you have parsed a term, you can start looking for `+` and \`\* operators.
I am tracking everything as a list, that way I can be sure to follow the order
of operations (PEMDAS) when building the final expression.
In one case, I need an operator and another term. If that happens I keep
looking for more. In the other case, I am done parsing, and I finalize the
expression.
-}
expressionHelp : List ( Expr, Operator ) -> Expr -> Parser Expr
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


type Operator
    = AddOp
    | SubtractOp
    | MultiplyOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> SubtractOp) (symbol "-")
        , map (\_ -> MultiplyOp) (symbol "*")
        ]


{-| We only have `+` and `*` in this parser. If we see a `MultiplyOp` we can
immediately group those two expressions. If we see an `AddOp` we wait to group
until all the multiplies have been taken care of.
This code is kind of tricky, but it is a baseline for what you would need if
you wanted to add `/`, `-`, `==`, `&&`, etc. which bring in more complex
associativity and precedence rules.
-}
finalize : List ( Expr, Operator ) -> Expr -> Expr
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( expr, MultiplyOp ) :: otherRevOps ->
            finalize otherRevOps (Multiply expr finalExpr)

        ( expr, AddOp ) :: otherRevOps ->
            Add (finalize otherRevOps expr) finalExpr

        ( expr, SubtractOp ) :: otherRevOps ->
            Subtract (finalize otherRevOps expr) finalExpr
