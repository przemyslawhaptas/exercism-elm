module Pangram exposing (isPangram)

import Dict exposing (..)


isPangram : String -> Bool
isPangram =
    String.foldl markOccurrence emptyLetterOccurredDict
        >> Dict.values
        >> List.all identity


type alias LetterOccurredDict =
    Dict Char Bool


markOccurrence : Char -> LetterOccurredDict -> LetterOccurredDict
markOccurrence letter dict =
    Dict.update (Char.toUpper letter) (Maybe.map (\_ -> True)) dict


emptyLetterOccurredDict : LetterOccurredDict
emptyLetterOccurredDict =
    letterRange 'A' 'Z'
        |> List.map (\letter -> ( letter, False ))
        |> Dict.fromList


letterRange : Char -> Char -> List Char
letterRange rangeStart rangeEnd =
    List.range (Char.toCode rangeStart) (Char.toCode rangeEnd)
        |> List.map Char.fromCode
