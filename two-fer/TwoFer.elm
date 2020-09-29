module TwoFer exposing (twoFer)


twoFer : Maybe String -> String
twoFer nameOrNothing =
    case nameOrNothing of
        Nothing ->
            "One for you, one for me."

        Just name ->
            "One for " ++ name ++ ", one for me."
