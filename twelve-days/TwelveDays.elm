module TwelveDays exposing (recite)

import Array exposing (Array)


dayOrders : Array String
dayOrders =
    Array.fromList
        [ ""
        , "first"
        , "second"
        , "third"
        , "fourth"
        , "fifth"
        , "sixth"
        , "seventh"
        , "eighth"
        , "ninth"
        , "tenth"
        , "eleventh"
        , "twelfth"
        ]


gifts : Array String
gifts =
    Array.fromList
        [ ""
        , "a Partridge"
        , "two Turtle Doves"
        , "three French Hens"
        , "four Calling Birds"
        , "five Gold Rings"
        , "six Geese-a-Laying"
        , "seven Swans-a-Swimming"
        , "eight Maids-a-Milking"
        , "nine Ladies Dancing"
        , "ten Lords-a-Leaping"
        , "eleven Pipers Piping"
        , "twelve Drummers Drumming"
        ]


recite : Int -> Int -> List String
recite start stop =
    List.map buildSentence (List.range start stop)


buildSentence : Int -> String
buildSentence dayNumber =
    "On the "
        ++ getDayOrder dayNumber
        ++ " day of Christmas my true love gave to me: "
        ++ (dayNumber |> getGiftsOnDay |> listGiftsOnDay)
        ++ " in a Pear Tree."


getDayOrder : Int -> String
getDayOrder dayNumber =
    case Array.get dayNumber dayOrders of
        Just order ->
            order

        Nothing ->
            ""


getGiftsOnDay : Int -> List String
getGiftsOnDay dayNumber =
    gifts
        |> Array.slice 1 (dayNumber + 1)
        |> Array.toList


listGiftsOnDay : List String -> String
listGiftsOnDay giftsOnDay =
    case giftsOnDay of
        [] ->
            ""

        firstDay :: [] ->
            firstDay

        firstDay :: rest ->
            (rest |> List.reverse |> String.join ", ")
                ++ ", and "
                ++ firstDay
