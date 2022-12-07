module Main exposing (main)

import Data
import Dict exposing (Dict)
import Element as El
import Element.Border as Border
import Element.Font as Font
import List.Extra as List
import Maybe.Extra as Maybe
import Parse exposing (..)
import Parser exposing ((|.), (|=))
import Result.Extra as Result
import Set
import Tree exposing (..)



-- Display the result in the browser


sequence input =
    Parser.run dataParser input |> Result.map (\d -> { size = directorySize d, children = d })


main =
    El.layout [] <|
        case sequence Data.data of
            Ok d ->
                let
                    diskSize =
                        70000000

                    remaining =
                        diskSize - d.size

                    needed =
                        30000000
                in
                El.column []
                    [ El.text ("input size: " ++ Debug.toString (String.length Data.data))
                    , El.text ("Directory size list: " ++ Debug.toString (toDirectorySizeList d.size d.children))
                    , El.text ("Part 1 answer: " ++ Debug.toString (toDirectorySizeList d.size d.children |> List.filter (\v -> v <= 100000) |> List.sum))
                    , El.text ("Part 2 answer: " ++ Debug.toString (toDirectorySizeList d.size d.children |> List.filter (\v -> v + remaining >= needed) |> List.minimum))

                    -- , El.text ("The answer to part 1 is: " ++ String.fromInt (d |> filterSize |> computeSize))
                    , viewDirectory "" d.size d.children
                    ]

            Err _ ->
                El.text "parse failed"


viewNode name node =
    case node.data of
        Dir { children } ->
            viewDirectory name node.size children

        File ->
            viewFile name node.size


viewDirectory name size directory =
    El.column [ Font.family [ Font.monospace ] ]
        [ El.text (name ++ "/ (" ++ Debug.toString size ++ ")")
        , El.column
            [ Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
            , Border.color <| El.rgb 0 0 0
            , El.paddingEach { left = 10, right = 0, top = 0, bottom = 0 }
            ]
            (directory
                |> Dict.toList
                |> List.map
                    (\( childname, node ) ->
                        viewNode childname node
                    )
            )
        ]


viewFile name size =
    El.row []
        [ El.el [ El.width (El.px 100) ] <| El.text (String.fromInt size)
        , El.el [ El.width (El.px 250) ] <| El.text name
        , El.text ("(" ++ Debug.toString size ++ ")")
        ]


toDirectorySizeList : Int -> Dict String Node -> List Int
toDirectorySizeList size children =
    let
        doChild n =
            case n.data of
                Dir data ->
                    n.size :: (data.children |> Dict.values |> List.map doChild |> List.concat)

                File ->
                    []
    in
    doChild { size = size, data = Dir { children = children } }
