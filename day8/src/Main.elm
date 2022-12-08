module Main exposing (main)

import Data
import Element as El
import Element.Font as Font
import Forest exposing (..)
import List.Extra as List



-- Display the result in the browser


main =
    let
        forest =
            Forest.compute Data.data
    in
    El.layout [] <|
        El.column []
            [ El.text ("input size: " ++ String.fromInt (String.length Data.data))
            , El.text ("Part 1 answer: " ++ String.fromInt (Forest.countVisible forest))
            , El.text ("Part 2 answer: " ++ String.fromInt (Forest.viewForestScore forest))
            , El.text
                "Forest: "
            , viewForest forest
            ]


viewForest forest =
    El.column [ Font.family [ Font.monospace ] ]
        (forest
            |> List.map
                (\row ->
                    row
                        |> List.map
                            (\tree ->
                                if Forest.isVisible tree then
                                    "((" ++ (String.fromInt <| Forest.treeScore tree) ++ "))"

                                else
                                    "  " ++ (String.fromInt <| Forest.treeScore tree) ++ "  "
                            )
                        |> String.concat
                        |> El.text
                )
        )
