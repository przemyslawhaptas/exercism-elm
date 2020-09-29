module RNATranscription exposing (toRNA)


type CommonNucleodite
    = G
    | C
    | A


type DNANucleodite
    = DNA CommonNucleodite
    | T


type RNANucleodite
    = RNA CommonNucleodite
    | U


type alias DNAStrand =
    List DNANucleodite


type alias RNAStrand =
    List RNANucleodite


toRNA : String -> Result String String
toRNA dnaString =
    case parseDNAString dnaString of
        Just dna ->
            Ok (complementDNA dna |> serializeRNA)

        Nothing ->
            Err "Invalid DNA string"


parseDNAString : String -> Maybe DNAStrand
parseDNAString =
    String.toList
        >> List.map
            (\dnaChar ->
                case dnaChar of
                    'G' ->
                        Just (DNA G)

                    'C' ->
                        Just (DNA C)

                    'T' ->
                        Just T

                    'A' ->
                        Just (DNA A)

                    _ ->
                        Nothing
            )
        >> foldMaybes


complementDNA : DNAStrand -> RNAStrand
complementDNA =
    List.map
        (\dnaNucleodite ->
            case dnaNucleodite of
                DNA G ->
                    RNA C

                DNA C ->
                    RNA G

                T ->
                    RNA A

                DNA A ->
                    U
        )


serializeRNA : RNAStrand -> String
serializeRNA =
    List.map
        (\rnaNucleodite ->
            case rnaNucleodite of
                RNA C ->
                    'C'

                RNA G ->
                    'G'

                U ->
                    'U'

                RNA A ->
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
