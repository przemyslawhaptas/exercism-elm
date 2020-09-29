module NucleotideCount exposing (nucleotideCounts)


type alias NucleotideCounts =
    { a : Int
    , t : Int
    , c : Int
    , g : Int
    }


nucleotideCounts : String -> NucleotideCounts
nucleotideCounts =
    String.foldl countNucleodite { a = 0, t = 0, c = 0, g = 0 }


countNucleodite : Char -> NucleotideCounts -> NucleotideCounts
countNucleodite nucleotide ({ a, t, c, g } as count) =
    case nucleotide of
        'A' ->
            { count | a = a + 1 }

        'T' ->
            { count | t = t + 1 }

        'C' ->
            { count | c = c + 1 }

        'G' ->
            { count | g = g + 1 }

        _ ->
            count
