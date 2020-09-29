module Wordy exposing (..)

import Parser exposing (..)


type Question
    = Question Operation


type Operation
    = Value Int
    | Addition Int Int
    | Subtraction Int Int
    | Multiplication Int Int
    | Division Int Int


type Operator
    = Add
    | Subtract
    | Multiply
    | Divide


answer : String -> Maybe Int
answer problem =
    Debug.todo "Please implement this function"


questionParser : Parser Question
questionParser =
    succeed Question
        |. keyword "What is"
        |. spaces
        |= operationParser
        |. keyword "?"


operationParser : Parser Operation
operationParser =
    oneOf
        [ succeed Addition
            |= int
            |. spaces
            |. symbol "+"
            |. spaces
            |= int
        , succeed Subtraction
            |= int
            |. spaces
            |. symbol "-"
            |. spaces
            |= int
        , succeed Multiplication
            |= int
            |. spaces
            |. symbol "*"
            |. spaces
            |= int
        , succeed Division
            |= int
            |. spaces
            |. symbol "/"
            |. spaces
            |= int
        , succeed Value
            |= int
        ]


operatorParser : Parser Operator
operatorParser =
    oneOf
        [ succeed Add
            |. keyword "plus"
        , succeed Subtract
            |. keyword "minus"
        , succeed Multiply
            |. keyword "multiplied by"
        , succeed Divide
            |. keyword "divided by"
        ]
