module Allergies exposing (Allergy(..), isAllergicTo, toList)

import Flip exposing (flip)


type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats


allergies : List Allergy
allergies =
    [ Eggs
    , Peanuts
    , Shellfish
    , Strawberries
    , Tomatoes
    , Chocolate
    , Pollen
    , Cats
    ]


toReversedBinary : Int -> List Int
toReversedBinary =
    toBinary_ []
        >> List.reverse


toBinary_ : List Int -> Int -> List Int
toBinary_ digits number =
    if number == 0 then
        digits

    else
        toBinary_ (remainderBy 2 number :: digits) (number // 2)


isAllergicTo : Allergy -> Int -> Bool
isAllergicTo =
    toList
        >> flip List.member
        |> flip


toList : Int -> List Allergy
toList =
    toReversedBinary
        >> List.map2 Tuple.pair allergies
        >> List.filter (Tuple.second >> (==) 1)
        >> List.map Tuple.first
