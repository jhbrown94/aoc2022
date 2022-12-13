module Main exposing (..)

import Data
import Dict
import Element as El
import Element.Font as Font
import List.Nonempty as Nonempty
import Monkey



-- Display the result in the browser


main =
    El.layout [] <|
        El.column []
            [ El.text ("Part 1 test data: " ++ Debug.toString (Data.testData |> Monkey.parse))
            , El.text ("Part 1 test answer: " ++ String.fromInt (Data.testData |> Monkey.parse |> Monkey.run 20 |> Monkey.monkeyBusiness))
            , El.text ("Part 1 REAL answer: " ++ String.fromInt (Data.data |> Monkey.parse |> Monkey.run 20 |> Monkey.monkeyBusiness))
            , El.text "Part 1 test snapshot: "
            , El.column [] (List.map (El.text << Debug.toString) (Data.testData |> Monkey.parse |> Monkey.run 20 |> Dict.toList))
            ]



-- , El.text ("Part 1 real solution: " ++ Debug.toString (real |> CPU.computeSignalStrength))
-- , El.text "Part 2 real solution:"
-- , El.column [ Font.family [ Font.monospace ] ] (CPU.computeScanLines real |> List.map El.text)
-- , El.text ("Part 1 history: " ++ Debug.toString (test |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
-- , El.text ("Part 1 history: " ++ Debug.toString (real |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
