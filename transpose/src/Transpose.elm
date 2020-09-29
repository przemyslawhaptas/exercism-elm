module Transpose exposing (transpose)


transpose : List String -> List String
transpose =
    List.map String.toList
        >> collectRows []
        >> List.map String.fromList


collectRows : List (List Char) -> List (List Char) -> List (List Char)
collectRows rows lines =
    let
        nextRows =
            appendUnlessEmpty (buildRow lines) rows
    in
    if List.all List.isEmpty lines then
        List.reverse nextRows

    else
        collectRows nextRows (getTails lines)


getTails : List (List Char) -> List (List Char)
getTails =
    List.map (List.tail >> Maybe.withDefault [])


buildRow : List (List Char) -> List Char
buildRow =
    List.map List.head
        >> (List.reverse >> trimLeftNothings >> List.reverse)
        >> List.map (Maybe.withDefault ' ')


trimLeftNothings : List (Maybe a) -> List (Maybe a)
trimLeftNothings list =
    case list of
        Nothing :: tail ->
            trimLeftNothings tail

        _ ->
            list


appendUnlessEmpty : List a -> List (List a) -> List (List a)
appendUnlessEmpty element list =
    case element of
        [] ->
            list

        _ ->
            element :: list
