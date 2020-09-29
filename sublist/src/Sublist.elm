module Sublist exposing (ListComparison(..), sublist)


type ListComparison
    = Equal
    | Superlist
    | Sublist
    | Unequal


sublist : List a -> List a -> ListComparison
sublist aList bList =
    if aList == bList then
        Equal

    else if contains aList bList then
        Superlist

    else if contains bList aList then
        Sublist

    else
        Unequal


contains : List a -> List a -> Bool
contains first second =
    contains_ first second first second


contains_ : List a -> List a -> List a -> List a -> Bool
contains_ firstScanned second firstAcc secondAcc =
    case ( firstScanned, firstAcc, secondAcc ) of
        ( _, _, [] ) ->
            True

        ( _, [], _ ) ->
            False

        ( [], _, _ ) ->
            False

        ( firstScannedH :: firstScannedT, firstAccH :: firstAccT, secondAccH :: secondAccT ) ->
            if firstAccH == secondAccH then
                contains_ firstScanned second firstAccT secondAccT

            else
                contains_ firstScannedT second firstScannedT second
