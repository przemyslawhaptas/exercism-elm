module WordCount exposing (wordCount)

import Dict exposing (Dict)
import Flip exposing (flip)
import Regex


wordCount : String -> Dict String Int
wordCount =
    Regex.split separator
        >> List.filter (String.length >> (<) 0)
        >> List.map String.toLower
        >> List.map clearCitations
        >> List.foldl (flip Dict.update updateCount) Dict.empty


separator : Regex.Regex
separator =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^(\\w+('\\w+)*)]"


citation : Regex.Regex
citation =
    Maybe.withDefault Regex.never <|
        Regex.fromString "'\\w+'"


clearCitations : String -> String
clearCitations =
    Regex.replace citation (.match >> String.dropLeft 1 >> String.dropRight 1)


updateCount : Maybe Int -> Maybe Int
updateCount =
    Maybe.withDefault 0 >> (+) 1 >> Just
