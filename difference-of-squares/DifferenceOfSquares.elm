module DifferenceOfSquares exposing (difference, squareOfSum, sumOfSquares)


elements : Int -> List Int
elements =
    List.range 1


square : Int -> Int
square n =
    n * n


addSquare : Int -> Int -> Int
addSquare el acc =
    square el + acc


sum : List Int -> Int
sum =
    List.foldl (+) 0


sumSquared : List Int -> Int
sumSquared =
    List.foldl addSquare 0


squareOfSum : Int -> Int
squareOfSum =
    elements >> sum >> square


sumOfSquares : Int -> Int
sumOfSquares =
    elements >> sumSquared


difference : Int -> Int
difference n =
    squareOfSum n - sumOfSquares n
