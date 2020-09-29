module Triangle exposing (Triangle(..), triangleKind)


type Triangle
    = Equilateral
    | Isosceles
    | Scalene


hasValidLengths : number -> number -> number -> Bool
hasValidLengths x y z =
    x > 0 && y > 0 && z > 0


isTriangle : number -> number -> number -> Bool
isTriangle x y z =
    (x + y > z) && (y + z > x) && (z + x > y)


isEquilateral : number -> number -> number -> Bool
isEquilateral x y z =
    (x == y) && (y == z)


isIsosceles : number -> number -> number -> Bool
isIsosceles x y z =
    (x == y) || (y == z) || (z == x)


triangleKind : number -> number -> number -> Result String Triangle
triangleKind x y z =
    if not (hasValidLengths x y z) then
        Err "Invalid lengths"

    else if not (isTriangle x y z) then
        Err "Violates inequality"

    else if isEquilateral x y z then
        Ok Equilateral

    else if isIsosceles x y z then
        Ok Isosceles

    else
        Ok Scalene
