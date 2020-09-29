module CollatzConjecture exposing (collatz, isEven)


collatz : Int -> Result String Int
collatz start =
    if start <= 0 then
        Err "Only positive numbers are allowed"

    else
        Ok (collatz_ start 0)


collatz_ : Int -> Int -> Int
collatz_ n step =
    if n == 1 then
        step

    else
        collatz_ (divideOrMultiply n) (step + 1)


divideOrMultiply : Int -> Int
divideOrMultiply n =
    if isEven n then
        n // 2

    else
        n * 3 + 1


isEven : Int -> Bool
isEven =
    modBy 2 >> (==) 0
