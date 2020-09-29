module Acronym exposing (abbreviate)

import Regex


abbreviate : String -> String
abbreviate =
    Regex.split separator
        >> List.filterMap String.uncons
        >> List.map (Tuple.first >> String.fromChar >> String.toUpper)
        >> String.join ""


separator : Regex.Regex
separator =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\W"
