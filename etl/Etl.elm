module Etl exposing (transform)

import Dict exposing (Dict)


transform : Dict Int (List String) -> Dict String Int
transform =
    Dict.toList
        >> List.concatMap zipFlip
        >> List.map (Tuple.mapFirst String.toLower)
        >> Dict.fromList


zipFlip : ( a, List b ) -> List ( b, a )
zipFlip ( value, elements ) =
    List.map (\element -> ( element, value )) elements
