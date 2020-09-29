module RomanNumerals exposing (toDigits, toRoman)


type alias PositionalNumeral =
    ( String, String, String )


positionalNumerals : List PositionalNumeral
positionalNumerals =
    [ ( "M", "", "" ) -- thousands
    , ( "C", "D", "M" ) -- hundreds
    , ( "X", "L", "C" ) -- tens
    , ( "I", "V", "X" ) -- ones
    ]


toRoman : Int -> String
toRoman =
    toDigits [] >> padZeroes 4 >> toRoman_


toRoman_ : List Int -> String
toRoman_ digits =
    List.map2 toRomanChunk digits positionalNumerals
        |> String.join ""


toRomanChunk : Int -> PositionalNumeral -> String
toRomanChunk digits ( numeral, halfNextNumeral, nextNumeral ) =
    String.join "" <|
        if digits < 4 then
            List.repeat digits numeral

        else if digits == 4 then
            [ numeral, halfNextNumeral ]

        else if digits < 9 then
            halfNextNumeral :: List.repeat (digits - 5) numeral

        else
            [ numeral, nextNumeral ]


padZeroes : Int -> List Int -> List Int
padZeroes length list =
    List.repeat (length - List.length list) 0 ++ list


toDigits : List Int -> Int -> List Int
toDigits acc number =
    if number == 0 then
        acc

    else
        toDigits (remainderBy 10 number :: acc) (number // 10)
