module CPU exposing (..)

import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import Maybe.Extra as Maybe


type alias Cpu =
    { x : Int }


type Instruction
    = Add Int
    | Noop


init =
    { x = 1 }


apply : Instruction -> Nonempty Cpu -> Nonempty Cpu
apply instruction history =
    let
        cpu =
            Nonempty.head history
    in
    case instruction of
        Add v ->
            Nonempty.cons { cpu | x = cpu.x + v } <|
                Nonempty.cons { cpu | x = cpu.x } history

        Noop ->
            Nonempty.cons cpu history


parseProgram : String -> List Instruction
parseProgram data =
    data
        |> String.split "\n"
        |> List.map (String.split " ")
        |> List.map
            (\instr ->
                case Debug.log "instruction string" instr of
                    [ "addx", v ] ->
                        Add (String.toInt v |> Maybe.withDefault 0)

                    [ "noop" ] ->
                        Noop

                    _ ->
                        Debug.todo "nope"
            )


run : String -> Nonempty Cpu
run data =
    let
        history =
            data
                |> parseProgram
                |> List.foldl (\instr cpu -> apply instr cpu) (Nonempty.singleton init)
                |> Nonempty.reverse
    in
    history


computeSignalStrength history =
    history
        |> Nonempty.toList
        |> List.indexedMap
            (\index cpu -> ( index + 1, cpu ))
        |> List.filterMap
            (\( cycle, cpu ) ->
                if modBy 40 (cycle + 20) == 0 then
                    Just <| cycle * cpu.x

                else
                    Nothing
            )
        |> List.sum


toScanLine history =
    history
        |> List.indexedMap
            (\xPos cpu ->
                if cpu.x - 1 <= xPos && cpu.x + 1 >= xPos then
                    '#'

                else
                    ' '
            )
        |> String.fromList


computeScanLines history =
    history
        |> Nonempty.toList
        |> List.groupsOf 40
        |> List.map toScanLine
