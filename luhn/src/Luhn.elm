module Luhn exposing (valid)


valid : String -> Bool
valid number =
    allNumbersOrSpaces number
        && longEnough (clear number)
        && checksumInTens (clear number)


allNumbersOrSpaces : String -> Bool
allNumbersOrSpaces =
    String.all (\c -> Char.isDigit c || c == ' ')


clear : String -> String
clear =
    String.filter Char.isDigit


longEnough : String -> Bool
longEnough string =
    String.length string > 1


checksumInTens : String -> Bool
checksumInTens string =
    remainderBy 10 (checksum string) == 0


checksum : String -> Int
checksum =
    String.toList
        >> List.map (String.fromChar >> String.toInt >> Maybe.withDefault 0)
        >> doubleEverySecond
        >> List.map trimAboveNine
        >> List.foldl (+) 0


doubleEverySecond : List Int -> List Int
doubleEverySecond numbers =
    List.reverse numbers |> doubleEverySecond_ [] |> List.reverse


doubleEverySecond_ : List Int -> List Int -> List Int
doubleEverySecond_ acc numbers =
    case numbers of
        a :: b :: tail ->
            doubleEverySecond_ (a :: b * 2 :: acc) tail

        a :: tail ->
            a :: acc

        _ ->
            acc


trimAboveNine : Int -> Int
trimAboveNine number =
    if number > 9 then
        number - 9

    else
        number
