module MatchingBrackets exposing (isPaired)

import Dict exposing (Dict)


isPaired : String -> Bool
isPaired =
    String.toList >> isPaired_ []


isPaired_ : List Char -> List Char -> Bool
isPaired_ bracketsAcc input =
    case input of
        [] ->
            List.isEmpty bracketsAcc

        inputH :: inputT ->
            if isOpeningBracket inputH then
                isPaired_ (inputH :: bracketsAcc) inputT

            else if isClosingBracket inputH then
                case bracketsAcc of
                    [] ->
                        False

                    bracketsH :: bracketsT ->
                        if isMatchingOpeningBracket bracketsH inputH then
                            isPaired_ bracketsT inputT

                        else
                            False

            else
                isPaired_ bracketsAcc inputT


bracketTags : Dict Char Char
bracketTags =
    Dict.fromList
        [ ( '(', ')' )
        , ( '{', '}' )
        , ( '[', ']' )
        ]


isOpeningBracket : Char -> Bool
isOpeningBracket char =
    List.member char (Dict.keys bracketTags)


isClosingBracket : Char -> Bool
isClosingBracket char =
    List.member char (Dict.values bracketTags)


isMatchingOpeningBracket : Char -> Char -> Bool
isMatchingOpeningBracket openingChar closingChar =
    case Dict.get openingChar bracketTags of
        Just closingTag ->
            closingTag == closingChar

        Nothing ->
            False
