module Main exposing (..)

import Data
import Element as El
import State



-- Display the result in the browser


main =
    El.layout [] <|
        El.column []
            [ El.text ("input size: " ++ String.fromInt (String.length Data.data))
            , El.text ("Part 1 answer: " ++ String.fromInt State.part1Result)
            , El.text ("Part 2 answer: " ++ String.fromInt State.part2Result)
            ]
