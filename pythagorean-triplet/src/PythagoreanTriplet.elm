module PythagoreanTriplet exposing (generateTriplets, triplets)


type alias Triplet =
    ( Int, Int, Int )


triplets : Int -> List Triplet
triplets sum =
    List.reverse (generateTriplets sum 1 2 3 [])


generateTriplets : Int -> Int -> Int -> Int -> List Triplet -> List Triplet
generateTriplets sum a b c acc =
    if a + b + c == sum then
        generateTriplets sum a (b + 1) (max (b + 2) (sum - a - (b + 1))) (addIfValid sum a b c acc)

    else if a + b + c > sum then
        if a + (b + 1) + (b + 2) > sum then
            if (a + 1) + (a + 2) + (a + 3) > sum then
                acc

            else
                generateTriplets sum (a + 1) (a + 2) (max (a + 3) (sum - (a + 1) - (a + 2))) acc

        else
            generateTriplets sum a (b + 1) (max (b + 2) (sum - a - (b + 1))) acc

    else
        generateTriplets sum a b (max (c + 1) (sum - a - b)) acc


addIfValid : Int -> Int -> Int -> Int -> List Triplet -> List Triplet
addIfValid sum a b c acc =
    if a ^ 2 + b ^ 2 == c ^ 2 then
        ( a, b, c ) :: acc

    else
        acc
