module ArmstrongNumbers exposing (isArmstrongNumber)

import Flip exposing (flip)


isArmstrongNumber : Int -> Bool
isArmstrongNumber nb =
    nb == (nb |> toDigits |> sumOfPowers)


toDigits : Int -> List Int
toDigits =
    String.fromInt
        >> String.split ""
        >> List.map (String.toInt >> Maybe.withDefault 0)


sumOfPowers : List Int -> Int
sumOfPowers numbers =
    List.foldl
        (flip (^) (List.length numbers) >> (+))
        0
        numbers
