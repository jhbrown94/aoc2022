module Monkey exposing (..)

import Dict exposing (Dict)


type MonkeyId
    = MonkeyId Int


type ItemWorry
    = ItemWorry Int


type alias Monkey =
    { id : MonkeyId
    , startsWith : List ItemWorry
    , operation : Operation
    , test : Test
    }


type Expr
    = IntExpr Int
    | Old


type Operator
    = Mul
    | Add


type alias Operation =
    { left : Expr
    , operator : Operator
    , right : Expr
    }


type alias Test =
    { divisibleBy : Int
    , trueMonkeyId : MonkeyId
    , falseMonkeyId : MonkeyId
    }


parse data =
    data |> String.split "\n\n" |> List.map parseMonkey


parseMonkey data =
    case String.split "\n" data of
        [ monkey, starting, operation, test, true, false ] ->
            Monkey
                (parseMonkeyId monkey)
                (parseStarting starting)
                (parseOperation operation)
                (Test (parseTest test) (parseTrue true) (parseFalse false))

        _ ->
            Debug.todo "Nope"


parseMonkeyId data =
    data |> String.dropLeft (String.length "Monkey ") |> String.dropRight 1 |> String.toInt |> Maybe.withDefault -1 |> MonkeyId


parseStarting data =
    data |> String.dropLeft (String.length "  Starting items: ") |> String.split ", " |> List.filterMap String.toInt |> List.map ItemWorry


parseTest data =
    data |> String.dropLeft (String.length "  Test: divisible by ") |> String.toInt |> Maybe.withDefault -1


parseOperation data =
    data |> String.dropLeft (String.length "  Operation: new = ") |> String.split " " |> parseOperationPhrase


parseOperationPhrase data =
    case data of
        [ left, op, right ] ->
            Operation (parseExpr left) (parseOperator op) (parseExpr right)

        _ ->
            Debug.todo ("Nope" ++ Debug.toString data)


parseExpr e =
    case e of
        "old" ->
            Old

        _ ->
            case String.toInt e of
                Just n ->
                    IntExpr n

                Nothing ->
                    Debug.todo "nope"


parseOperator op =
    case op of
        "*" ->
            Mul

        "+" ->
            Add

        _ ->
            Debug.todo "nope"


parseTrue data =
    data |> String.dropLeft (String.length "    If true: throw to monkey ") |> String.toInt |> Maybe.withDefault -1 |> MonkeyId


parseFalse data =
    data |> String.dropLeft (String.length "    If false: throw to monkey ") |> String.toInt |> Maybe.withDefault -1 |> MonkeyId


type alias ItemMap =
    Dict Int MonkeyRecord


type alias MonkeyRecord =
    { items : List ItemWorry
    , count : Int
    }


monkeyIdToInt (MonkeyId v) =
    v


initialConfiguration : List Monkey -> ( List Monkey, ItemMap )
initialConfiguration monkeys =
    ( monkeys, monkeys |> List.map (\monkey -> ( monkey.id |> monkeyIdToInt, { items = monkey.startsWith, count = 0 } )) |> Dict.fromList )


run n monkeys =
    doRounds n (initialConfiguration monkeys) |> Tuple.second


doRounds n configuration =
    if n <= 0 then
        configuration

    else
        doRounds (n - 1) (advance configuration)


advance ( monkeys, itemMap ) =
    ( monkeys, monkeys |> List.foldl advanceMonkey itemMap )


advanceMonkey : Monkey -> ItemMap -> ItemMap
advanceMonkey monkey itemMap =
    let
        monkeyRecord =
            case itemMap |> Dict.get (monkey.id |> monkeyIdToInt) of
                Just r ->
                    r

                Nothing ->
                    Debug.todo "No monkeyrecord"
    in
    monkeyRecord.items
        |> List.foldl (advanceItem monkey monkeyRecord)
            (itemMap |> Dict.insert (monkey.id |> monkeyIdToInt) { items = [], count = monkeyRecord.count + List.length monkeyRecord.items })


advanceItem : Monkey -> MonkeyRecord -> ItemWorry -> ItemMap -> ItemMap
advanceItem monkey monkeyRecord item itemMap =
    let
        _ =
            Debug.log "MONKEY ID: " monkey.id

        worry =
            monkeyInspects item monkey.operation |> applyRelief |> Debug.log "Worry "

        applyRelief (ItemWorry w) =
            w // 3 |> ItemWorry

        target =
            doThrow worry

        doThrow (ItemWorry w) =
            if modBy monkey.test.divisibleBy w == 0 then
                monkey.test.trueMonkeyId

            else
                monkey.test.falseMonkeyId

        targetInt =
            monkeyIdToInt target

        targetRecord =
            case Dict.get targetInt itemMap of
                Just v ->
                    v

                Nothing ->
                    Debug.todo "No target monkey record"
    in
    itemMap |> Dict.insert targetInt { targetRecord | items = worry :: targetRecord.items }


monkeyInspects : ItemWorry -> Operation -> ItemWorry
monkeyInspects (ItemWorry worry) operation =
    let
        _ =
            Debug.log "op" operation

        l =
            case operation.left of
                IntExpr v ->
                    v

                Old ->
                    worry

        r =
            case operation.right of
                IntExpr v ->
                    v

                Old ->
                    worry

        op =
            case operation.operator of
                Mul ->
                    (*)

                Add ->
                    (+)
    in
    op l r |> ItemWorry |> Debug.log "Monkey inspected"


monkeyBusiness itemMap =
    let
        topTwo =
            itemMap
                |> Dict.values
                |> List.map .count
                |> List.sortBy negate
                |> List.take 2
    in
    case topTwo of
        [ a, b ] ->
            a * b

        _ ->
            Debug.todo "Nope"
