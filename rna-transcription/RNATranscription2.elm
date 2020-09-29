module RNATranscription exposing (toRNA)


type DNANucleodite
    = DNA_G
    | DNA_C
    | DNA_T
    | DNA_A


type RNANucleodite
    = RNA_C
    | RNA_G
    | RNA_U
    | RNA_A


type alias DNA =
    List DNANucleodite


type alias RNA =
    List RNANucleodite


toRNA : String -> Result String String
toRNA dnaString =
    case parseDNAString dnaString of
        Just dna ->
            Ok (complementDNA dna |> serializeRNA)

        Nothing ->
            Err "Invalid DNA string"


parseDNAString : String -> Maybe DNA
parseDNAString =
    String.toList
        >> List.map
            (\dnaChar ->
                case dnaChar of
                    'G' ->
                        Just DNA_G

                    'C' ->
                        Just DNA_C

                    'T' ->
                        Just DNA_T

                    'A' ->
                        Just DNA_A

                    _ ->
                        Nothing
            )
        >> foldMaybes


complementDNA : DNA -> RNA
complementDNA =
    List.map
        (\dnaNucleodite ->
            case dnaNucleodite of
                DNA_G ->
                    RNA_C

                DNA_C ->
                    RNA_G

                DNA_T ->
                    RNA_A

                DNA_A ->
                    RNA_U
        )


serializeRNA : RNA -> String
serializeRNA =
    List.map
        (\rnaNucleodite ->
            case rnaNucleodite of
                RNA_C ->
                    'C'

                RNA_G ->
                    'G'

                RNA_U ->
                    'U'

                RNA_A ->
                    'A'
        )
        >> String.fromList


foldMaybes : List (Maybe a) -> Maybe (List a)
foldMaybes =
    List.foldl
        (\maybe maybeList ->
            case maybe of
                Just el ->
                    Maybe.map ((::) el) maybeList

                Nothing ->
                    Nothing
        )
        (Just [])
        >> Maybe.map List.reverse
