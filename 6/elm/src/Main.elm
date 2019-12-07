module Main exposing (..)

import Dict exposing (Dict)
import List.Extra


parse : List String -> Dict String String
parse segments =
    segments
        |> List.map (\s -> String.split ")" s)
        |> List.map
            (\pair ->
                case pair of
                    a :: b :: _ ->
                        Just ( a, b )

                    _ ->
                        Nothing
            )
        |> List.foldl
            (\maybeTuple acc ->
                case maybeTuple of
                    Just ( parent, child ) ->
                        Dict.insert child parent acc

                    Nothing ->
                        acc
            )
            Dict.empty


orbitCount : List String -> Int
orbitCount segments =
    segments
        |> parse
        |> makeAncestors
        |> Dict.values
        |> List.map List.length
        |> List.sum


ancestors : List String -> String -> List String
ancestors segments key =
    segments
        |> parse
        |> makeAncestors
        |> Dict.get key
        |> Maybe.withDefault []


transfers : List String -> String -> String -> List String
transfers input source dest =
    let
        state =
            parse input
                |> makeAncestors

        src =
            (Dict.get source state |> Maybe.withDefault []) |> List.reverse

        dst =
            (Dict.get dest state |> Maybe.withDefault []) |> List.reverse

        srcl =
            List.length src

        dstl =
            List.length dst

        ( src_, dst_ ) =
            if srcl < dstl then
                ( src ++ List.repeat (dstl - srcl) "", dst )

            else
                ( src, dst ++ List.repeat (srcl - dstl) "" )

        pairs =
            List.map2 Tuple.pair src_ dst_
    in
    List.Extra.dropWhile (\( a, b ) -> a == b) pairs
        |> List.concatMap
            (\( a, b ) ->
                [ a, b ]
            )
        |> List.filter (\s -> s /= "")


makeAncestors : Dict String String -> Dict String (List String)
makeAncestors dict =
    dict
        |> Dict.keys
        |> List.foldl
            (\k acc ->
                Dict.insert k (getAncestors k dict) acc
            )
            Dict.empty


getAncestors : String -> Dict String String -> List String
getAncestors key state =
    case Dict.get key state of
        Nothing ->
            []

        Just v ->
            v :: getAncestors v state
