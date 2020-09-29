module AllYourBase exposing (rebase)

import Flip exposing (flip)


rebase : Int -> List Int -> Int -> Maybe (List Int)
rebase inBase digits outBase =
    if invalidInputs inBase digits outBase then
        Nothing

    else
        Just (rebase_ inBase digits outBase)


invalidInputs : Int -> List Int -> Int -> Bool
invalidInputs inBase digits outBase =
    (digits == [])
        || List.all ((==) 0) digits
        || List.any (\d -> d < 0 || d >= inBase) digits
        || (inBase < 2)
        || (outBase < 2)


rebase_ : Int -> List Int -> Int -> List Int
rebase_ inBase digits outBase =
    toTenBased inBase digits
        |> fromTenBased outBase


toTenBased : Int -> List Int -> Int
toTenBased inBase digits =
    List.reverse digits
        |> List.indexedMap ((^) inBase >> (*))
        |> List.foldl (+) 0


fromTenBased : Int -> Int -> List Int
fromTenBased =
    fromTenBased_ []


fromTenBased_ : List Int -> Int -> Int -> List Int
fromTenBased_ digits outBase number =
    if number == 0 then
        digits

    else
        fromTenBased_ (remainderBy outBase number :: digits) outBase (number // outBase)
