module ListOps exposing
    ( append
    , concat
    , filter
    , foldl
    , foldr
    , length
    , map
    , reverse
    )


length : List a -> Int
length list =
    length_ list 0


length_ : List a -> Int -> Int
length_ list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            length_ tail (acc + 1)


reverse : List a -> List a
reverse list =
    reverse_ list []


reverse_ list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            reverse_ tail (head :: acc)


foldl : (a -> b -> b) -> b -> List a -> b
foldl f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            foldl f (f head acc) tail


foldr : (a -> b -> b) -> b -> List a -> b
foldr f acc list =
    case list of
        [] ->
            acc

        head :: tail ->
            f head (foldr f acc tail)


map : (a -> b) -> List a -> List b
map f list =
    map_ f list []


map_ f list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            map_ f tail (acc ++ [ f head ])


filter : (a -> Bool) -> List a -> List a
filter f list =
    filter_ f list []


filter_ f list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            filter_ f tail (ifElse (f head) (acc ++ [ head ]) acc)


append : List a -> List a -> List a
append xs ys =
    append_ xs ys xs


append_ xs ys acc =
    case ys of
        [] ->
            acc

        head :: tail ->
            append_ xs tail (acc ++ [ head ])


concat : List (List a) -> List a
concat list =
    concat_ list []


concat_ list acc =
    case list of
        [] ->
            acc

        head :: tail ->
            concat_ tail (acc ++ head)


ifElse : Bool -> a -> a -> a
ifElse predicate whenTrue whenFalse =
    if predicate then
        whenTrue

    else
        whenFalse
