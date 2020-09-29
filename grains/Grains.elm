module Grains exposing (square, total)


decrement a =
    a - 1


square : Int -> Maybe Int
square n =
    if n > 0 then
        Just (square_ n)

    else
        Nothing


square_ : Int -> Int
square_ =
    decrement >> (^) 2


total : Int -> Maybe Int
total n =
    if n > 0 then
        Just (List.range 1 n |> List.map square_ |> List.foldl (+) 0)

    else
        Nothing
