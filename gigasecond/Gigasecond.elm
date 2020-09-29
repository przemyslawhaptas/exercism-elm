module Gigasecond exposing (add)

import Time


type alias Millisecond =
    Int


type alias Second =
    Int


type alias Gigasecond =
    Int


gigasecondToSeconds : Gigasecond -> Second
gigasecondToSeconds =
    (*) (10 ^ 9)


secondToMilliseconds : Second -> Millisecond
secondToMilliseconds =
    (*) (10 ^ 3)


gigasecondToMilliseconds : Gigasecond -> Millisecond
gigasecondToMilliseconds =
    gigasecondToSeconds >> secondToMilliseconds


add : Time.Posix -> Time.Posix
add =
    Time.posixToMillis >> (+) (gigasecondToMilliseconds 1) >> Time.millisToPosix
