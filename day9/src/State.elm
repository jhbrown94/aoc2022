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
    List Point


start : Int -> State
start n =
    List.repeat n (Point 0 0)


type Move
    = Up
    | Down
    | Left
    | Right


apply : Move -> State -> State
apply move state =
    let
        head =
            List.head state |> Maybe.withDefault (Point 0 0)

        rest =
            List.drop 1 state

        newHead =
            case move of
                Up ->
                    addY 1 head

                Down ->
                    addY -1 head

                Left ->
                    addX -1 head

                Right ->
                    addX 1 head

        helper follower ( leader, accum ) =
            let
                deltaX =
                    leader.x - follower.x

                adjustX =
                    if deltaX /= 0 then
                        deltaX // abs deltaX

                    else
                        0

                deltaY =
                    leader.y - follower.y

                adjustY =
                    if deltaY /= 0 then
                        deltaY // abs deltaY

                    else
                        0
            in
            case ( abs deltaX, abs deltaY ) of
                ( 2, _ ) ->
                    ( { x = follower.x + adjustX, y = follower.y + adjustY }, leader :: accum )

                ( _, 2 ) ->
                    ( { x = follower.x + adjustX, y = follower.y + adjustY }, leader :: accum )

                _ ->
                    ( follower, leader :: accum )
    in
    List.foldl helper ( newHead, [] ) rest |> (\( last, accum ) -> last :: accum |> List.reverse) |> Debug.log "New state:"


compute size moveList =
    moveList
        |> List.foldl
            (\move ( state, tailSet ) ->
                let
                    next =
                        apply move state

                    tail =
                        next |> List.reverse |> List.head |> Maybe.withDefault (Point 0 0)
                in
                ( next, Set.insert ( tail.x, tail.y ) tailSet )
            )
            ( start size, Set.singleton ( 0, 0 ) )
        |> Tuple.second
        |> Set.size


parseCommands data =
    data
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
    compute 2 (parseCommands Data.data)


part2Result =
    compute 10 (parseCommands Data.data)
