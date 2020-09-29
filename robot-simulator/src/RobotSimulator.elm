module RobotSimulator exposing
    ( Bearing(..)
    , Robot
    , advance
    , defaultRobot
    , simulate
    , turnLeft
    , turnRight
    )


type Bearing
    = North
    | East
    | South
    | West


type alias Robot =
    { bearing : Bearing
    , coordinates : { x : Int, y : Int }
    }


defaultRobot : Robot
defaultRobot =
    Robot North { x = 0, y = 0 }


turnRight : Robot -> Robot
turnRight { bearing, coordinates } =
    case bearing of
        North ->
            Robot East coordinates

        East ->
            Robot South coordinates

        South ->
            Robot West coordinates

        West ->
            Robot North coordinates


turnLeft : Robot -> Robot
turnLeft { bearing, coordinates } =
    case bearing of
        North ->
            Robot West coordinates

        East ->
            Robot North coordinates

        South ->
            Robot East coordinates

        West ->
            Robot South coordinates


advance : Robot -> Robot
advance { bearing, coordinates } =
    case bearing of
        North ->
            Robot bearing { coordinates | y = coordinates.y + 1 }

        East ->
            Robot bearing { coordinates | x = coordinates.x + 1 }

        South ->
            Robot bearing { coordinates | y = coordinates.y - 1 }

        West ->
            Robot bearing { coordinates | x = coordinates.x - 1 }


mapDirection : Char -> Robot -> Robot
mapDirection letter =
    case letter of
        'R' ->
            turnRight

        'L' ->
            turnLeft

        'A' ->
            advance

        _ ->
            identity


simulate : String -> Robot -> Robot
simulate directions robot =
    String.toList directions |> List.foldl mapDirection robot
