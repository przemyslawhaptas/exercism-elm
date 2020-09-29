module Bob exposing (hey)

import Char exposing (isAlpha, isUpper)
import String exposing (all, endsWith, filter, isEmpty, toUpper, trim)


both : (a -> Bool) -> (a -> Bool) -> a -> Bool
both left right arg =
    left arg && right arg


nonEmpty : String -> Bool
nonEmpty =
    isEmpty >> not


isYelling : String -> Bool
isYelling =
    filter isAlpha >> both nonEmpty (all isUpper)


isQuestion : String -> Bool
isQuestion =
    endsWith "?"


isYellingQuestion : String -> Bool
isYellingQuestion =
    both isQuestion isYelling


isSilence : String -> Bool
isSilence =
    isEmpty


respond : String -> String
respond remark =
    if isSilence remark then
        "Fine. Be that way!"

    else if isYellingQuestion remark then
        "Calm down, I know what I'm doing!"

    else if isYelling remark then
        "Whoa, chill out!"

    else if isQuestion remark then
        "Sure."

    else
        "Whatever."


hey : String -> String
hey =
    trim >> respond
