module Raindrops exposing (raindrops)

-- Why so many occurrences of `number`? Refactor in spare time.


raindrops : Int -> String
raindrops number =
    raindropSpeak number |> orStraightNumber number


raindropSpeak : Int -> String
raindropSpeak number =
    ""
        |> withFactor 3 "Pling" number
        |> withFactor 5 "Plang" number
        |> withFactor 7 "Plong" number


withFactor : Int -> String -> Int -> String -> String
withFactor factor raindropWord number response =
    if isFactor factor number then
        response ++ raindropWord

    else
        response


orStraightNumber : Int -> String -> String
orStraightNumber number response =
    if String.isEmpty response then
        String.fromInt number

    else
        response


isFactor : Int -> Int -> Bool
isFactor number =
    modBy number >> (==) 0
