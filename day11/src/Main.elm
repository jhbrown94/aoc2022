module Main exposing (..)

import Browser
import Data
import Dict
import Element as El
import Element.Font as Font
import Html
import List.Nonempty as Nonempty
import Monkey
import Process
import Task



-- Display the result in the browser


type alias Model =
    { count : Int
    , config : Monkey.Configuration
    }


type Msg
    = Iterate


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        monkeys =
            Data.data |> Monkey.parse
    in
    ( { count = 0, config = Monkey.init False monkeys }, doNext )


doNext =
    Process.sleep (Debug.log "delay" 10) |> Task.perform (always Iterate)


increment =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Iterate ->
            if model.count < 10000 then
                ( { model | count = model.count + increment, config = Monkey.doRounds increment model.config }, doNext )

            else
                ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    El.layout [] <|
        El.column []
            [ El.text ("Part 1 test data: " ++ Debug.toString (Data.testData |> Monkey.parse))
            , El.text ("Part 1 test answer: " ++ String.fromInt (Data.testData |> Monkey.parse |> Monkey.run True 20 |> Monkey.monkeyBusiness))
            , El.text ("Part 1 REAL answer: " ++ String.fromInt (Data.data |> Monkey.parse |> Monkey.run True 20 |> Monkey.monkeyBusiness))

            --, El.text ("Part 2  test answer: " ++ String.fromInt (Data.testData |> Monkey.parse |> Monkey.run False 10 |> Monkey.monkeyBusiness))
            , El.text ("Part 2 REAL answer: " ++ String.fromInt model.count ++ " " ++ "Monkey biz: " ++ String.fromInt (model.config.itemMap |> Monkey.monkeyBusiness))
            , El.text "Part 2 evolving monkeys: "
            , El.column []
                (List.map (\m -> El.text (Debug.toString m))
                    (model.config.itemMap
                        |> Dict.toList
                    )
                )
            , El.text "Part 1 test snapshot: "
            , El.column [] (List.map (El.text << Debug.toString) (Data.testData |> Monkey.parse |> Monkey.run True 20 |> Dict.toList))

            --, El.text "Part 2 test snapshot: "
            --, El.column [] (List.map (El.text << Debug.toString) (Data.testData |> Monkey.parse |> Monkey.run False 20 |> Dict.toList))
            ]



-- , El.text ("Part 1 real solution: " ++ Debug.toString (real |> CPU.computeSignalStrength))
-- , El.text "Part 2 real solution:"
-- , El.column [ Font.family [ Font.monospace ] ] (CPU.computeScanLines real |> List.map El.text)
-- , El.text ("Part 1 history: " ++ Debug.toString (test |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
-- , El.text ("Part 1 history: " ++ Debug.toString (real |> Nonempty.indexedMap (\i c -> ( i + 1, c ))))
