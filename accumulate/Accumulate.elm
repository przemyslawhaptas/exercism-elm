module Accumulate exposing (accumulate)


accumulate : (a -> b) -> List a -> List b
accumulate =
    accumulate_ []


accumulate_ : List b -> (a -> b) -> List a -> List b
accumulate_ output func input =
    case input of
        [] ->
            List.reverse output

        head :: tail ->
            accumulate_ (func head :: output) func tail
