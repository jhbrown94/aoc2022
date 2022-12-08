module Forest exposing (..)

import Dict exposing (Dict)
import List.Extra as List


type alias Tree =
    { height : Int
    , visibleFrom : VisibleFrom
    , viewDistance : ViewDistance
    }


type alias VisibleFrom =
    { north : Bool, south : Bool, east : Bool, west : Bool }


type alias ViewDistance =
    { north : Int, south : Int, east : Int, west : Int }


isVisible tree =
    let
        f =
            tree.visibleFrom
    in
    f.north || f.south || f.east || f.west


treeScore t =
    t.viewDistance.north * t.viewDistance.south * t.viewDistance.east * t.viewDistance.west


type alias Forest =
    List (List Tree)


compute : String -> Forest
compute data =
    let
        rowLines =
            data |> String.split "\n"

        rowParser rowLine =
            rowLine
                |> String.split ""
                |> List.filterMap String.toInt
                |> List.map
                    (\height ->
                        { height = height
                        , visibleFrom = { north = False, south = False, east = False, west = False }
                        , viewDistance = { north = 0, south = 0, east = 0, west = 0 }
                        }
                    )

        base : Forest
        base =
            rowLines |> List.map rowParser

        sweep : (VisibleFrom -> VisibleFrom) -> Forest -> Forest
        sweep setter input =
            let
                handleCell : Tree -> ( Int, List Tree ) -> ( Int, List Tree )
                handleCell tree ( visHeight, trees ) =
                    if visHeight < tree.height then
                        ( tree.height, { tree | visibleFrom = setter tree.visibleFrom } :: trees )

                    else
                        ( visHeight, tree :: trees )

                handleRow : List Tree -> List Tree
                handleRow row =
                    List.foldl handleCell ( -1, [] ) row |> Tuple.second |> List.reverse
            in
            input
                |> List.map handleRow

        sweepDistance : (Int -> ViewDistance -> ViewDistance) -> Forest -> Forest
        sweepDistance setter input =
            let
                handleCell : Tree -> ( Int, VisRecord, List Tree ) -> ( Int, VisRecord, List Tree )
                handleCell tree ( index, visRecord, trees ) =
                    let
                        newVisRecord =
                            updateVis tree.height index visRecord
                    in
                    ( index + 1, newVisRecord, { tree | viewDistance = setter (index - getVisIndex tree.height visRecord) tree.viewDistance } :: trees )

                handleRow : List Tree -> List Tree
                handleRow row =
                    List.foldl handleCell ( 0, Dict.empty, [] ) row |> (\( _, _, c ) -> c) |> List.reverse
            in
            input
                |> List.map handleRow
    in
    base
        |> sweep (\t -> { t | west = True })
        |> sweepDistance (\n t -> { t | west = n })
        |> List.map List.reverse
        |> sweep (\t -> { t | east = True })
        |> sweepDistance (\n t -> { t | east = n })
        |> List.map List.reverse
        |> List.transpose
        |> sweep (\t -> { t | north = True })
        |> sweepDistance (\n t -> { t | north = n })
        |> List.map List.reverse
        |> sweep (\t -> { t | south = True })
        |> sweepDistance (\n t -> { t | south = n })
        |> List.map List.reverse
        |> List.transpose


type alias VisRecord =
    Dict Int Int


updateVis : Int -> Int -> VisRecord -> VisRecord
updateVis height index visRecord =
    List.range 0 height |> List.foldl (\h vr -> Dict.insert h index vr) visRecord


getVisIndex height visRecord =
    Dict.get height visRecord |> Maybe.withDefault 0


countVisible forest =
    forest |> List.map (List.filter isVisible >> List.length) |> List.sum


viewForestScore : Forest -> Int
viewForestScore forest =
    forest
        |> List.filterMap
            (\row ->
                row
                    |> List.map treeScore
                    |> List.maximum
            )
        |> List.maximum
        |> Maybe.withDefault -1
