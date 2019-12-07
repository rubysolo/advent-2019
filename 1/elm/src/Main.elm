module Main exposing (..)


fuel : Int -> Int
fuel weight =
    toFloat weight
        / 3
        |> floor
        |> (\x -> x - 2)


rFuel : Int -> Int
rFuel weight =
    rFuelHelp weight []


rFuelHelp : Int -> List Int -> Int
rFuelHelp weight acc =
    let
        f =
            fuel weight
    in
    if f < 1 then
        List.sum acc

    else
        rFuelHelp f (f :: acc)
