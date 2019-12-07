module Main exposing (..)

import List.Extra exposing (group)


findValid : Int -> Int -> List Int
findValid a z =
    let
        candidates =
            List.range a z

        toChars cand =
            String.fromInt cand
                |> String.toList
    in
    candidates
        |> List.filter
            (\c ->
                let
                    chars =
                        toChars c
                in
                double chars && monotonic chars
            )


double : List Char -> Bool
double chars =
    let
        runs =
            group chars
                |> List.map (\( h, t ) -> h :: t)
    in
    List.any (\r -> List.length r == 2) runs


monotonic : List Char -> Bool
monotonic chars =
    eachPair chars
        |> List.all (\( a, b ) -> a <= b)


eachTriple : List a -> List ( a, a, a )
eachTriple xs =
    zip xs (zip (List.drop 1 xs) (List.drop 2 xs))
        |> List.map (\( a, ( b, c ) ) -> ( a, b, c ))


eachPair : List a -> List ( a, a )
eachPair xs =
    zip xs (List.drop 1 xs)


zip : List a -> List b -> List ( a, b )
zip xs ys =
    List.map2 Tuple.pair xs ys
