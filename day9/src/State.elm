module State exposing (..)

import Data
import Set exposing (Set)


type alias Point =
    { x : Int, y : Int }


addX delta point =
    { point | x = point.x + delta }


addY delta point =
    { point | y = point.y + delta }


type alias State =
    { head : Point
    , tail : Point
    }


start : State
start =
    { head = Point 0 0, tail = Point 0 0 }


type Move
    = Up
    | Down
    | Left
    | Right


apply move state =
    let
        headMoved =
            case move of
                Up ->
                    { state | head = addY 1 state.head }

                Down ->
                    { state | head = addY -1 state.head }

                Left ->
                    { state | head = addX -1 state.head }

                Right ->
                    { state | head = addX 1 state.head }

        delta =
            { x = headMoved.head.x - headMoved.tail.x, y = headMoved.head.y - headMoved.tail.y }
    in
    case ( abs delta.x, abs delta.y ) of
        ( 2, d ) ->
            { headMoved | tail = { x = headMoved.tail.x + delta.x // 2, y = headMoved.tail.y + delta.y } }

        ( d, 2 ) ->
            { headMoved | tail = { x = headMoved.tail.x + delta.x, y = headMoved.tail.y + delta.y // 2 } }

        _ ->
            headMoved


computePart1 moveList =
    moveList
        |> List.foldl
            (\move ( state, tailSet ) ->
                let
                    next =
                        apply move state
                in
                ( next, Set.insert ( next.tail.x, next.tail.y ) tailSet )
            )
            ( start, Set.singleton ( start.tail.x, start.tail.y ) )
        |> Tuple.second
        |> Set.size


parseCommands =
    Data.data
        |> String.split "\n"
        |> List.map
            (\line ->
                case line |> String.split " " of
                    [ cmd, count ] ->
                        let
                            c =
                                String.toInt count |> Maybe.withDefault 0
                        in
                        List.repeat c
                            (case cmd of
                                "U" ->
                                    Up

                                "D" ->
                                    Down

                                "L" ->
                                    Left

                                "R" ->
                                    Right

                                _ ->
                                    Up
                            )

                    _ ->
                        Debug.todo "Nope"
            )
        |> List.concat


part1Result =
    computePart1 parseCommands
