module AtbashCipher exposing (decode, encode)


groupSize =
    5


encode : String -> String
encode =
    String.filter Char.isAlphaNum
        >> String.toLower
        >> String.map flipIfLetter
        >> groupInFives
        >> String.join " "


decode : String -> String
decode =
    String.filter Char.isAlphaNum
        >> String.map flipIfLetter


flipIfLetter : Char -> Char
flipIfLetter letter =
    if Char.isAlpha letter then
        (Char.toCode 'z' - (Char.toCode letter - Char.toCode 'a'))
            |> Char.fromCode

    else
        letter


groupInFives : String -> List String
groupInFives =
    groupInFives_ []


groupInFives_ : List String -> String -> List String
groupInFives_ groups string =
    case string of
        "" ->
            List.reverse groups

        _ ->
            groupInFives_
                (String.left groupSize string :: groups)
                (String.dropLeft groupSize string)
