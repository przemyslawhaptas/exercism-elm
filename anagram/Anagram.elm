module Anagram exposing (detect)

import Dict exposing (Dict)


equalLetterCount : String -> String -> Bool
equalLetterCount word candidate =
    List.sort (String.toList word) == List.sort (String.toList candidate)


detect : String -> List String -> List String
detect word candidates =
    candidates
        |> List.filter (\candidate -> String.toLower word /= String.toLower candidate)
        |> List.filter (\candidate -> equalLetterCount (String.toLower word) (String.toLower candidate))
