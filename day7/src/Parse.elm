module Parse exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=))
import Result.Extra as Result
import Set exposing (Set)
import Tree exposing (..)


type Command
    = Cd String
    | Ls (List Entry)


type Entry
    = FileEntry FileData
    | DirEntry DirData


type alias FileData =
    { size : Int
    , name : String
    }


type alias DirData =
    { name : String
    }


commandParser =
    Parser.oneOf [ cdParser, lsParser ]


cdParser =
    Parser.succeed Cd
        |. Parser.token "$ cd "
        |= fileNameParser
        |. newLine


fileNameParser =
    Parser.variable
        { start = \c -> Char.isAlpha c || c == '.' || c == '/'
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '.' || c == '/'
        , reserved = Set.empty
        }


newLine =
    Parser.oneOf [ Parser.chompIf (\c -> c == '\n'), Parser.end ]


lsParser =
    Parser.succeed Ls
        |. Parser.token "$ ls"
        |. newLine
        |= fileListParser


fileParser =
    Parser.succeed FileData
        |= Parser.int
        |. Parser.spaces
        |= fileNameParser
        |. newLine
        |> Parser.map FileEntry


dirParser =
    Parser.succeed DirData
        |. Parser.token "dir "
        |= fileNameParser
        |. newLine
        |> Parser.map DirEntry


entryParser =
    Parser.oneOf
        [ fileParser
        , dirParser
        ]


fileListParser =
    let
        helper entries =
            Parser.oneOf
                [ entryParser |> Parser.map (\e -> Parser.Loop (e :: entries))
                , Parser.succeed <| Parser.Done (List.reverse entries)
                ]
    in
    Parser.loop [] helper


dataParser : Parser.Parser Directory
dataParser =
    let
        helper zipper =
            Parser.oneOf
                [ commandParser |> Parser.map (\c -> Parser.Loop (applyCommand c zipper))
                , Parser.succeed <| Parser.Done (eject zipper)
                ]
    in
    Parser.loop (Dict.empty |> toZipper "/") helper |. Parser.end


applyCommand command zipper =
    case command of
        Cd "/" ->
            toTop zipper

        Cd ".." ->
            ascend zipper

        Cd name ->
            descend name zipper

        Ls entries ->
            updateEntries entries zipper


updateEntries entries zipper =
    zipper
        |> Tree.setChildren
            (entries
                |> List.map
                    (\entry ->
                        case entry of
                            FileEntry fe ->
                                ( fe.name, { size = fe.size, data = File } )

                            DirEntry fe ->
                                ( fe.name, { size = 0, data = Dir { children = Dict.empty } } )
                    )
                |> Dict.fromList
            )
