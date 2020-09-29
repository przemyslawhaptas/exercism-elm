module Wordy exposing (answer)

import Parser exposing (..)



{-
   Based on the elm/parser's Math example.
   Not suitable for large expressions as neither
   `operatorsAndExpressions` or `buildExpressions`
   are tail-recursive.
-}


type Question
    = Question Expression


type Expression
    = Value Int
    | Addition Expression Expression
    | Subtraction Expression Expression
    | Multiplication Expression Expression
    | Division Expression Expression


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide


answer : String -> Maybe Int
answer problem =
    case parse problem of
        Ok (Question expr) ->
            Just (evaluate expr)

        Err _ ->
            Nothing


evaluate : Expression -> Int
evaluate expr =
    case expr of
        Value n ->
            n

        Addition a b ->
            evaluate a + evaluate b

        Subtraction a b ->
            evaluate a - evaluate b

        Multiplication a b ->
            evaluate a * evaluate b

        Division a b ->
            evaluate a // evaluate b


parse : String -> Result (List DeadEnd) Question
parse string =
    run question string


question : Parser Question
question =
    succeed Question
        |. keyword "What is"
        |. spaces
        |= expression
        |. keyword "?"


expression : Parser Expression
expression =
    value
        |> andThen (operatorsAndExpressions [])


value : Parser Expression
value =
    succeed Value
        |= myInt


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


operatorsAndExpressions : List ( Expression, Operator ) -> Expression -> Parser Expression
operatorsAndExpressions reversedOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= value
            |> andThen
                (\( op, lastExpr ) ->
                    operatorsAndExpressions (( expr, op ) :: reversedOps) lastExpr
                )
        , lazy (\_ -> succeed (buildExpressions reversedOps expr))
        ]


operator : Parser Operator
operator =
    oneOf
        [ succeed Add |. symbol "plus"
        , succeed Subtract |. symbol "minus"
        , succeed Multiply |. symbol "multiplied by"
        , succeed Divide |. symbol "divided by"
        ]


buildExpressions : List ( Expression, Operator ) -> Expression -> Expression
buildExpressions reversedOps lastExpr =
    case reversedOps of
        [] ->
            lastExpr

        ( expr, Add ) :: reversedOpsTail ->
            Addition (buildExpressions reversedOpsTail expr) lastExpr

        ( expr, Subtract ) :: reversedOpsTail ->
            Subtraction (buildExpressions reversedOpsTail expr) lastExpr

        ( expr, Multiply ) :: reversedOpsTail ->
            Multiplication (buildExpressions reversedOpsTail expr) lastExpr

        ( expr, Divide ) :: reversedOpsTail ->
            Division (buildExpressions reversedOpsTail expr) lastExpr
