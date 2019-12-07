module Main exposing (..)

import List.Extra exposing (getAt, setAt)


findSolution : Int -> String -> List ( Int, Int, Int )
findSolution final initialState =
    let
        values =
            List.range 0 99

        pairs =
            List.Extra.lift2 Tuple.pair values values

        state =
            parse initialState

        allResults =
            List.map (\pair -> generate pair state) pairs
    in
    List.filter (\( n, v, r ) -> r == final) allResults


generate : ( Int, Int ) -> List Int -> ( Int, Int, Int )
generate ( noun, verb ) initialState =
    let
        state =
            initialState
                |> setAt 1 noun
                |> setAt 2 verb

        result =
            run_ state
    in
    ( noun, verb, result )


findSolution_ : List ( Int, Int ) -> Int -> List Int -> ( Int, Int )
findSolution_ candidates final initialState =
    case candidates of
        ( noun, verb ) :: rest ->
            let
                state =
                    initialState
                        |> setAt 1 noun
                        |> setAt 2 verb

                result =
                    run_ state

                log =
                    Debug.log "" <| String.fromInt noun ++ " " ++ String.fromInt verb ++ " " ++ String.fromInt result
            in
            --if result == final then
            --    ( noun, verb )
            --else
            findSolution_ rest final initialState

        [] ->
            let
                log =
                    Debug.log "ERROR" "no solution"
            in
            ( -1, -1 )


run : String -> Int
run initial =
    parse initial
        |> execute 0
        |> List.head
        |> Maybe.withDefault -1


run_ : List Int -> Int
run_ initial =
    initial
        |> execute 0
        |> List.head
        |> Maybe.withDefault -1


parse : String -> List Int
parse input =
    String.split "," input
        |> List.map String.toInt
        |> List.map (Maybe.withDefault -1)


execute : Int -> List Int -> List Int
execute pos state =
    let
        op =
            valueAt state pos
    in
    next op pos state


next : Int -> Int -> List Int -> List Int
next op pos state =
    case op of
        1 ->
            continue (\l r -> l + r) pos state

        2 ->
            continue (\l r -> l * r) pos state

        99 ->
            state

        _ ->
            []


continue : (Int -> Int -> Int) -> Int -> List Int -> List Int
continue fn pos state =
    let
        left =
            valueAt state <| valueAt state (pos + 1)

        right =
            valueAt state <| valueAt state (pos + 2)

        target =
            valueAt state (pos + 3)

        nextState =
            updatedState target (fn left right) state
    in
    execute (pos + 4) nextState


updatedState : Int -> Int -> List Int -> List Int
updatedState address value state =
    setAt address value state


valueAt : List Int -> Int -> Int
valueAt vals pos =
    getAt pos vals
        |> Maybe.withDefault -1
