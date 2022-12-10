module Main exposing (..)

import CPU
import Data
import Element as El
import Element.Font as Font
import List.Nonempty as Nonempty



-- Display the result in the browser


main =
    let
        test =
            CPU.run Data.testData

        real =
            CPU.run Data.data
    in
    El.layout [] <|
        El.column []
            [ El.text ("Part 1 test solution: " ++ Debug.toString (test |> CPU.computeSignalStrength))
            , El.text ("Part 1 real solution: " ++ Debug.toString (real |> CPU.computeSignalStrength))
            , El.text "Part 2 real solution:"
            , El.column [ Font.family [ Font.monospace ] ] (CPU.computeScanLines real |> List.map El.text)
            , El.text ("Part 1 history: " ++ Debug.toString (test |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
            , El.text ("Part 1 history: " ++ Debug.toString (real |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
            ]
