module Leap exposing (isLeapYear)

import Result


div : Int -> Int -> Bool
div n =
    modBy n >> (==) 0


isLeapYear : Int -> Bool
isLeapYear year =
    div 4 year
        && (not (div 100 year) || div 400 year)
