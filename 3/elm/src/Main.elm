module Main exposing (..)

import List.Extra
import Set


type alias Point =
    ( Int, Int )


showPoint : Point -> String
showPoint ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Move =
    { direction : Direction
    , distance : Int
    }


crossingDistance : String -> String -> Int
crossingDistance wire1 wire2 =
    let
        s1 =
            Set.fromList <| allPoints wire1

        s2 =
            Set.fromList <| allPoints wire2

        common =
            Set.intersect s1 s2
                |> Set.remove ( 0, 0 )

        distances =
            List.map (\( x, y ) -> abs x + abs y) <| Set.toList common
    in
    List.minimum distances
        |> Maybe.withDefault -1


render : String -> String
render wire =
    let
        ms =
            moves wire

        path =
            List.map svgMove ms
                |> String.join " "
    in
    "<path d=\"M 0 0 " ++ path ++ "\" fill=\"transparent\" stroke=\"black\"/>"


svgMove : Move -> String
svgMove { direction, distance } =
    case direction of
        Up ->
            "v " ++ String.fromInt distance

        Down ->
            "v " ++ (String.fromInt <| distance * -1)

        Left ->
            "h " ++ (String.fromInt <| distance * -1)

        Right ->
            "h " ++ String.fromInt distance


minWireDistance : String -> String -> Int
minWireDistance wire1 wire2 =
    let
        p1 =
            allPoints wire1

        p2 =
            allPoints wire2

        s1 =
            Set.fromList p1

        s2 =
            Set.fromList p2

        r1 =
            Debug.log "[path1]" <| render wire1

        r2 =
            Debug.log "[path2]" <| render wire2

        common =
            Debug.log "[common]"
                (Set.intersect s1 s2
                    |> Set.remove ( 0, 0 )
                )

        distances =
            List.map (\point -> totalWireDistance point p1 p2) <| Set.toList common
    in
    List.minimum distances
        |> Maybe.withDefault -1


wireDistance : Point -> List Point -> Int
wireDistance point wire =
    List.Extra.elemIndices point wire
        |> List.head
        |> Maybe.withDefault 999999999


totalWireDistance : Point -> List Point -> List Point -> Int
totalWireDistance point wire1 wire2 =
    wireDistance point wire1 + wireDistance point wire2


allPoints : String -> List Point
allPoints path =
    let
        ( _, points ) =
            allPoints_ ( 0, 0 ) (moves path) []
    in
    points
        |> List.Extra.uniqueBy showPoint


allPoints_ : Point -> List Move -> List Point -> ( Point, List Point )
allPoints_ ( x, y ) steps points =
    case steps of
        { direction, distance } :: rest ->
            case direction of
                Up ->
                    let
                        ys =
                            List.range y (y + distance)

                        newPoints =
                            List.map (\ny -> ( x, ny )) ys

                        newPos =
                            ( x, y + distance )
                    in
                    allPoints_ newPos rest <| points ++ newPoints

                Down ->
                    let
                        ys =
                            List.range (y - distance) y
                                |> List.reverse

                        newPoints =
                            List.map (\ny -> ( x, ny )) ys

                        newPos =
                            ( x, y - distance )
                    in
                    allPoints_ newPos rest <| points ++ newPoints

                Left ->
                    let
                        xs =
                            List.range (x - distance) x
                                |> List.reverse

                        newPoints =
                            List.map (\nx -> ( nx, y )) xs

                        newPos =
                            ( x - distance, y )
                    in
                    allPoints_ newPos rest <| points ++ newPoints

                Right ->
                    let
                        xs =
                            List.range x (x + distance)

                        newPoints =
                            List.map (\nx -> ( nx, y )) xs

                        newPos =
                            ( x + distance, y )
                    in
                    allPoints_ newPos rest <| points ++ newPoints

        [] ->
            ( ( x, y ), points )


moves : String -> List Move
moves input =
    String.split "," input
        |> List.map String.uncons
        |> List.map move


move : Maybe ( Char, String ) -> Move
move input =
    case input of
        Nothing ->
            Move Up 0

        Just ( dir, dist ) ->
            Move (parseDir dir) (parseDist dist)


parseDir : Char -> Direction
parseDir input =
    case input of
        'U' ->
            Up

        'D' ->
            Down

        'L' ->
            Left

        _ ->
            Right


parseDist : String -> Int
parseDist s =
    String.toInt s
        |> Maybe.withDefault 0
