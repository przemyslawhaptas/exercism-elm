module SumOfMultiples exposing (sumOfMultiples)

import Set exposing (foldl, fromList)


findMultiples : Int -> Int -> List Int
findMultiples =
    findMultiples_ 1 []


findMultiples_ : Int -> List Int -> Int -> Int -> List Int
findMultiples_ multiplier multiples limit num =
    let
        nextMultiple =
            num * multiplier
    in
    if nextMultiple < limit then
        findMultiples_ (multiplier + 1) (nextMultiple :: multiples) limit num

    else
        multiples


sumOfMultiples : List Int -> Int -> Int
sumOfMultiples nums limit =
    nums
        |> List.concatMap (findMultiples limit)
        |> Set.fromList
        |> Set.foldl (+) 0
