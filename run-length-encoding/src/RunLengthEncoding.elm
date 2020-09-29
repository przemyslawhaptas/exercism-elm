module RunLengthEncoding exposing (buildOccurrences, decode, encode, findSerializedOccurrences)


type alias Occurrence =
    ( Char, Int )


encode : String -> String
encode =
    buildOccurrences >> List.map serializeOccurrence >> String.concat


buildOccurrences : String -> List Occurrence
buildOccurrences =
    String.reverse >> String.toList >> buildOccurrences_ []


buildOccurrences_ : List Occurrence -> List Char -> List Occurrence
buildOccurrences_ occurrences chars =
    case chars of
        [] ->
            occurrences

        char :: charTail ->
            buildOccurrences_
                (case occurrences of
                    [] ->
                        ( char, 1 ) :: occurrences

                    ( lastChar, lastCharCount ) :: occurrencesTail ->
                        if char == lastChar then
                            ( char, lastCharCount + 1 ) :: occurrencesTail

                        else
                            ( char, 1 ) :: occurrences
                )
                charTail


serializeOccurrence : Occurrence -> String
serializeOccurrence ( char, count ) =
    if count == 1 then
        String.fromChar char

    else
        String.fromInt count ++ String.fromChar char


decode : String -> String
decode =
    findSerializedOccurrences
        >> List.map deserializeOccurrence
        >> decodeOccurrences
        >> String.fromList


findSerializedOccurrences : String -> List String
findSerializedOccurrences =
    String.toList
        >> findSerializedOccurrences_ [] ""
        >> List.reverse


findSerializedOccurrences_ : List String -> String -> List Char -> List String
findSerializedOccurrences_ acc currentChunk chars =
    case chars of
        head :: tail ->
            if Char.isDigit head then
                findSerializedOccurrences_
                    acc
                    (String.cons head currentChunk)
                    tail

            else
                findSerializedOccurrences_
                    ((String.cons head currentChunk |> String.reverse) :: acc)
                    ""
                    tail

        [] ->
            acc


deserializeOccurrence : String -> Occurrence
deserializeOccurrence chunk =
    case String.uncons (String.reverse chunk) of
        Just ( char, countAsStringReversed ) ->
            ( char, countAsStringReversed |> String.reverse |> String.toInt |> Maybe.withDefault 1 )

        Nothing ->
            ( ' ', 0 )


decodeOccurrences : List Occurrence -> List Char
decodeOccurrences occurrences =
    List.concatMap (\( char, count ) -> List.repeat count char) occurrences
