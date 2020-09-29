module Strain exposing (discard, keep)


keep : (a -> Bool) -> List a -> List a
keep =
    List.filter


discard : (a -> Bool) -> List a -> List a
discard =
    List.filter << (<<) not
