module LargestSeriesProduct exposing (largestProduct)

import Array exposing (Array)
import Regex


largestProduct : Int -> String -> Maybe Int
largestProduct span series =
    if validInputs span series then
        Just (largestProduct_ 0 span (parseNumbers series) 0)

    else
        Nothing


validInputs : Int -> String -> Bool
validInputs span series =
    if span > String.length series then
        False

    else if span < 0 then
        False

    else if Regex.contains nonDigit series then
        False

    else
        True


nonDigit : Regex.Regex
nonDigit =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\D"


parseNumbers : String -> Array Int
parseNumbers series =
    String.toList series
        |> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)
        |> Array.fromList


largestProduct_ : Int -> Int -> Array Int -> Int -> Int
largestProduct_ startIndex endIndex numbers currentGreatest =
    let
        greatest =
            greaterOf currentGreatest (product startIndex endIndex numbers)
    in
    if endIndex == Array.length numbers then
        greatest

    else
        largestProduct_ (startIndex + 1) (endIndex + 1) numbers greatest


greaterOf : Int -> Int -> Int
greaterOf a b =
    if a >= b then
        a

    else
        b


product : Int -> Int -> Array Int -> Int
product startIndex endIndex numbers =
    Array.slice startIndex endIndex numbers
        |> Array.foldl (*) 1
