module PhoneNumber exposing (getNumber)

import Array exposing (Array)
import Flip exposing (flip)


type alias Digits =
    Array Int


getNumber : String -> Maybe String
getNumber phoneNumber =
    case validatePhoneNumber (filterDigits phoneNumber) of
        ( True, digits ) ->
            Just (digitsToString digits)

        _ ->
            Nothing


filterDigits : String -> Digits
filterDigits =
    String.split "" >> List.filterMap String.toInt >> Array.fromList


validatePhoneNumber : Digits -> ( Bool, Digits )
validatePhoneNumber digits =
    case Array.length digits of
        11 ->
            ( validateCountryCode (Array.slice 0 1 digits)
                && validatePlainPhoneNumber (Array.slice 1 11 digits)
            , Array.slice 1 11 digits
            )

        10 ->
            ( validatePlainPhoneNumber (Array.slice 0 10 digits)
            , Array.slice 0 10 digits
            )

        _ ->
            ( False, Array.empty )


validatePlainPhoneNumber : Digits -> Bool
validatePlainPhoneNumber digits =
    List.all identity
        [ validateAreaCode (Array.slice 0 2 digits)
        , validateExchangeCode (Array.slice 3 6 digits)
        , validateSubscriberNumber (Array.slice 7 10 digits)
        ]


validateCountryCode : Digits -> Bool
validateCountryCode =
    Array.get 0
        >> Maybe.map ((==) 1)
        >> Maybe.withDefault False


validateAreaCode : Digits -> Bool
validateAreaCode =
    Array.get 0
        >> Maybe.map (flip (>) 1)
        >> Maybe.withDefault False


validateExchangeCode : Digits -> Bool
validateExchangeCode =
    validateAreaCode


validateSubscriberNumber _ =
    True


digitsToString : Digits -> String
digitsToString =
    Array.foldl (String.fromInt >> flip (++)) ""
