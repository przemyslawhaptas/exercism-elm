module Isogram exposing (isIsogram)

import Dict exposing (..)


isIsogram : String -> Bool
isIsogram =
    String.foldl markOccurrence emptyLetterCount
        >> Dict.values
        >> List.all (\a -> a <= 1)


type alias LetterCount =
    Dict Char Int


markOccurrence : Char -> LetterCount -> LetterCount
markOccurrence letter count =
    Dict.update (Char.toUpper letter) (Maybe.map ((+) 1)) count


emptyLetterCount : LetterCount
emptyLetterCount =
    letterRange 'A' 'Z'
        |> List.map (\letter -> ( letter, 0 ))
        |> Dict.fromList


letterRange : Char -> Char -> List Char
letterRange rangeStart rangeEnd =
    List.range (Char.toCode rangeStart) (Char.toCode rangeEnd)
        |> List.map Char.fromCode
