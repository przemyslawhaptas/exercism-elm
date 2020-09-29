module Series exposing (slices)

import Array exposing (Array)


slices : Int -> String -> Result String (List (List Int))
slices size input =
    if String.length input == 0 then
        Err "series cannot be empty"

    else if size > String.length input then
        Err "slice length cannot be greater than series length"

    else if size == 0 then
        Err "slice length cannot be zero"

    else if size < 0 then
        Err "slice length cannot be negative"

    else
        Ok (slices_ 0 size (toNumbers input) [])


slices_ : Int -> Int -> Array Int -> List (List Int) -> List (List Int)
slices_ startIndex endIndex input acc =
    if endIndex > Array.length input then
        acc

    else
        slices_ (startIndex + 1) (endIndex + 1) input <|
            (acc ++ [ Array.toList <| Array.slice startIndex endIndex input ])


toNumbers : String -> Array Int
toNumbers =
    String.split ""
        >> List.map (String.toInt >> Maybe.withDefault 0)
        >> Array.fromList
