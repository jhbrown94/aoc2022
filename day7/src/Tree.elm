module Tree exposing (..)

import Dict exposing (Dict)
import Maybe.Extra as Maybe


type alias Node =
    { size : Int
    , data : NodeData
    }


type NodeData
    = Dir { children : Directory }
    | File


type alias Directory =
    Dict String Node


directorySize dir =
    dir |> Dict.values |> List.map .size |> List.sum


type Zipper
    = Zipper ZipperRep


type alias ZipperRep =
    { parent : Maybe Zipper
    , name : String
    , children : Directory
    }


setChildren children (Zipper origin) =
    Zipper { origin | children = children }


toZipper : String -> Directory -> Zipper
toZipper name directory =
    Zipper <| ZipperRep Nothing name directory


descend : String -> Zipper -> Zipper
descend name ((Zipper origin) as zipper) =
    let
        parent =
            Zipper { origin | children = Dict.remove name origin.children }
    in
    case Dict.get name origin.children of
        Just node ->
            case node.data of
                Dir { children } ->
                    Zipper <| ZipperRep (Just parent) name children

                File ->
                    zipper

        Nothing ->
            zipper


ascend : Zipper -> Zipper
ascend ((Zipper origin) as zipper) =
    origin.parent
        |> Maybe.map
            (\(Zipper parent) ->
                Zipper
                    { parent
                        | children =
                            parent.children
                                |> Dict.insert origin.name
                                    { size = origin.children |> directorySize
                                    , data = Dir { children = origin.children }
                                    }
                    }
            )
        |> Maybe.withDefault zipper


toTop ((Zipper origin) as zipper) =
    if Maybe.isJust origin.parent then
        zipper |> ascend |> toTop

    else
        zipper


eject zipper =
    zipper |> toTop |> fromZipper


fromZipper : Zipper -> Directory
fromZipper (Zipper zipper) =
    zipper.children
