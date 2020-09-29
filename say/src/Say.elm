module Say exposing (SayError(..), say, splitIntoChunksOfThousands)


type SayError
    = Negative
    | TooLarge


minimum =
    0


maximum =
    10 ^ 12


say : Int -> Result SayError String
say number =
    if number < minimum then
        Err Negative

    else if number > maximum then
        Err TooLarge

    else
        Ok (say_ number)


say_ : Int -> String
say_ number =
    let
        chunks =
            splitIntoChunksOfThousands number
    in
    if number == 0 then
        "zero"

    else
        chunks
            |> List.indexedMap (sayChunk (List.length chunks))
            |> nameChunksOfThousands
            |> List.filter (not << String.isEmpty)
            |> String.join " "


sayChunk : Int -> Int -> Int -> String
sayChunk chunksLength index chunk =
    sayUnderThousand_ chunk (chunksLength > 1 && index == chunksLength - 1)


nameChunksOfThousands : List String -> List String
nameChunksOfThousands chunks =
    [ "", "thousand", "million", "billion" ]
        |> List.map2
            (\chunk name ->
                if String.isEmpty chunk then
                    ""

                else if String.isEmpty name then
                    chunk

                else
                    String.concat [ chunk, " ", name ]
            )
            (List.reverse chunks)
        |> List.reverse


splitIntoChunksOfThousands : Int -> List Int
splitIntoChunksOfThousands =
    String.fromInt
        >> String.toList
        >> List.reverse
        >> splitIntoThrees []
        >> List.reverse
        >> List.map (List.reverse >> String.fromList >> String.toInt >> Maybe.withDefault 0)


splitIntoThrees : List (List a) -> List a -> List (List a)
splitIntoThrees acc list =
    case list of
        [] ->
            List.reverse acc

        a :: b :: c :: tail ->
            splitIntoThrees ([ a, b, c ] :: acc) tail

        fewerElements ->
            splitIntoThrees (fewerElements :: acc) []


sayUnderThousand_ : Int -> Bool -> String
sayUnderThousand_ number lastOfManyChunks =
    let
        hundreds =
            sayHundreds (number // 100)

        underHundred =
            sayUnderHundred_ (remainderBy 100 number)
    in
    if String.isEmpty hundreds then
        if String.isEmpty underHundred then
            ""

        else if lastOfManyChunks then
            "and " ++ underHundred

        else
            underHundred

    else if String.isEmpty underHundred then
        hundreds

    else
        hundreds ++ " and " ++ underHundred


sayUnderHundred_ : Int -> String
sayUnderHundred_ number =
    if number > 10 && number < 20 then
        sayTeens (remainderBy 10 number)

    else
        [ sayTens (number // 10)
        , sayOnes (remainderBy 10 number)
        ]
            |> List.filter (not << String.isEmpty)
            |> String.join "-"


sayOnes : Int -> String
sayOnes number =
    case number of
        1 ->
            "one"

        2 ->
            "two"

        3 ->
            "three"

        4 ->
            "four"

        5 ->
            "five"

        6 ->
            "six"

        7 ->
            "seven"

        8 ->
            "eight"

        9 ->
            "nine"

        _ ->
            ""


sayTeens : Int -> String
sayTeens number =
    case number of
        1 ->
            "eleven"

        2 ->
            "twelve"

        3 ->
            "thirteen"

        4 ->
            "fourteen"

        5 ->
            "fifteen"

        6 ->
            "sixteen"

        7 ->
            "seventeen"

        8 ->
            "eighteen"

        9 ->
            "nineteen"

        _ ->
            ""


sayTens : Int -> String
sayTens number =
    case number of
        1 ->
            "ten"

        2 ->
            "twenty"

        3 ->
            "thirty"

        4 ->
            "forty"

        5 ->
            "fifty"

        6 ->
            "sixty"

        7 ->
            "seventy"

        8 ->
            "eighty"

        9 ->
            "ninety"

        _ ->
            ""


sayHundreds : Int -> String
sayHundreds number =
    if number == 0 then
        ""

    else
        sayOnes number ++ " hundred"
