module ScrabbleScore exposing (scoreWord)

import Dict exposing (Dict)


zipFlip : ( a, List b ) -> List ( b, a )
zipFlip ( value, elements ) =
    List.map (\element -> ( element, value )) elements


valueToLetterDict : Dict Int (List Char)
valueToLetterDict =
    Dict.fromList
        [ ( 1, [ 'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T' ] )
        , ( 2, [ 'D', 'G' ] )
        , ( 3, [ 'B', 'C', 'M', 'P' ] )
        , ( 4, [ 'F', 'H', 'V', 'W', 'Y' ] )
        , ( 5, [ 'K' ] )
        , ( 8, [ 'J', 'X' ] )
        , ( 10, [ 'Q', 'Z' ] )
        ]


letterLookup : Dict Char Int
letterLookup =
    Dict.toList valueToLetterDict
        |> List.map zipFlip
        |> List.foldl (++) []
        |> Dict.fromList


letterValue : Char -> Int
letterValue letter =
    Dict.get (Char.toUpper letter) letterLookup
        |> Maybe.withDefault 0


scoreWord : String -> Int
scoreWord =
    String.toList
        >> List.map letterValue
        >> List.foldl (+) 0
