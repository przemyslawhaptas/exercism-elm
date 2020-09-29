module Triangle exposing (rows)


rows : Int -> List (List Int)
rows =
    List.map buildRow << List.range 1


buildRow : Int -> List Int
buildRow n =
    case n of
        0 ->
            []

        1 ->
            [ 1 ]

        _ ->
            List.map2 (+) ([ 0 ] ++ buildRow (n - 1)) (buildRow (n - 1) ++ [ 0 ])
