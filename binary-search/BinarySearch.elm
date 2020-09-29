module BinarySearch exposing (find)

import Array exposing (Array)


type alias Index =
    Int


find : Int -> Array Int -> Maybe Index
find target xs =
    find_ target xs 0


find_ : Int -> Array Int -> Index -> Maybe Index
find_ target xs arrayIndex =
    findMiddle xs
        |> Maybe.andThen (findOrPartition target xs arrayIndex)


findOrPartition : Int -> Array Int -> Index -> Int -> Maybe Index
findOrPartition target xs arrayIndex middleElement =
    let
        middleElementIndex =
            findMiddleIndex xs

        ( leftSlice, rightSlice ) =
            partition xs middleElementIndex
    in
    if target < middleElement then
        find_ target leftSlice arrayIndex

    else if target > middleElement then
        find_ target rightSlice (arrayIndex + middleElementIndex + 1)

    else
        Just (arrayIndex + middleElementIndex)


findMiddle : Array Int -> Maybe Int
findMiddle xs =
    Array.get (findMiddleIndex xs) xs


findMiddleIndex : Array Int -> Index
findMiddleIndex xs =
    Array.length xs // 2


partition : Array Int -> Index -> ( Array Int, Array Int )
partition xs index =
    ( Array.slice 0 index xs
    , Array.slice (index + 1) (Array.length xs) xs
    )
