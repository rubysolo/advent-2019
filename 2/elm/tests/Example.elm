module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "intcode"
        [ test "1,0,0,0,99" <|
            \_ ->
                let
                    input =
                        "1,0,0,0,99"

                    result =
                        run input
                in
                Expect.equal 2 result
        , test "1,9,10,3,2,3,11,0,99,30,40,50" <|
            \_ ->
                let
                    input =
                        "1,9,10,3,2,3,11,0,99,30,40,50"

                    result =
                        run input
                in
                Expect.equal 3500 result
        , test "1,1,1,4,99,5,6,0,99" <|
            \_ ->
                let
                    input =
                        "1,1,1,4,99,5,6,0,99"

                    result =
                        run input
                in
                Expect.equal 30 result
        , test "the real one" <|
            \_ ->
                let
                    input =
                        "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,9,23,27,2,27,6,31,1,5,31,35,2,9,35,39,2,6,39,43,2,43,13,47,2,13,47,51,1,10,51,55,1,9,55,59,1,6,59,63,2,63,9,67,1,67,6,71,1,71,13,75,1,6,75,79,1,9,79,83,2,9,83,87,1,87,6,91,1,91,13,95,2,6,95,99,1,10,99,103,2,103,9,107,1,6,107,111,1,10,111,115,2,6,115,119,1,5,119,123,1,123,13,127,1,127,5,131,1,6,131,135,2,135,13,139,1,139,2,143,1,143,10,0,99,2,0,14,0"

                    result =
                        run input
                in
                Expect.equal 6568671 result
        , test "find the solution" <|
            \_ ->
                let
                    input =
                        "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,9,23,27,2,27,6,31,1,5,31,35,2,9,35,39,2,6,39,43,2,43,13,47,2,13,47,51,1,10,51,55,1,9,55,59,1,6,59,63,2,63,9,67,1,67,6,71,1,71,13,75,1,6,75,79,1,9,79,83,2,9,83,87,1,87,6,91,1,91,13,95,2,6,95,99,1,10,99,103,2,103,9,107,1,6,107,111,1,10,111,115,2,6,115,119,1,5,119,123,1,123,13,127,1,127,5,131,1,6,131,135,2,135,13,139,1,139,2,143,1,143,10,0,99,2,0,14,0"

                    result =
                        findSolution 19690720 input
                in
                Expect.equal [ ( 39, 51, 19690720 ) ] result

        -- 429 is too low
        -- 441 is too low
        ]
