module Hamming exposing (distance)


distance : String -> String -> Result String Int
distance left right =
    if String.length left == String.length right then
        Ok (distance_ (String.toList left) (String.toList right) 0)

    else
        Err "left and right strands must be of equal length"


distance_ : List Char -> List Char -> Int -> Int
distance_ left right count =
    case ( left, right ) of
        ( leftHead :: leftTail, rightHead :: rightTail ) ->
            distance_ leftTail rightTail (countDifference leftHead rightHead count)

        ( _, _ ) ->
            count


countDifference : Char -> Char -> Int -> Int
countDifference leftLetter rightLetter count =
    if leftLetter == rightLetter then
        count

    else
        count + 1
