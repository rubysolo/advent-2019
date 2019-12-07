module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (double, findValid, monotonic)
import Test exposing (..)


suite : Test
suite =
    describe "day 4"
        [ test "monotonic 111111" <|
            \_ ->
                Expect.equal True (monotonic [ '1', '1', '1', '1', '1', '1' ])
        , test "monotonic 223450" <|
            \_ ->
                Expect.equal False (monotonic [ '2', '2', '3', '4', '5', '0' ])
        , test "monotonic 123789" <|
            \_ ->
                Expect.equal True (monotonic [ '1', '2', '3', '7', '8', '9' ])
        , test "double 111111" <|
            \_ ->
                Expect.equal False (double [ '1', '1', '1', '1', '1', '1' ])
        , test "double 111122" <|
            \_ ->
                Expect.equal True (double [ '1', '1', '1', '1', '2', '2' ])
        , test "double 112233" <|
            \_ ->
                Expect.equal True (double [ '1', '1', '2', '2', '3', '3' ])
        , test "double 123444" <|
            \_ ->
                Expect.equal False (double [ '1', '2', '3', '4', '4', '4' ])
        , test "double 223450" <|
            \_ ->
                Expect.equal True (double [ '2', '2', '3', '4', '5', '0' ])
        , test "double 123789" <|
            \_ ->
                Expect.equal False (double [ '1', '2', '3', '7', '8', '9' ])
        , test "filter" <|
            \_ ->
                Expect.equal 0 <| List.length <| findValid 234208 765869
        ]
