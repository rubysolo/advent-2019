module Main exposing (..)

import List.Extra exposing (getAt, setAt)


type alias State =
    { state : List Int
    , instructionPointer : Int
    , inputs : List Int
    , outputs : List Int
    , errors : List String
    , log : List Instruction
    }


type alias TrinaryParams =
    { left : Int
    , right : Int
    , target : Int
    }


type alias BinaryParams =
    { left : Int
    , right : Int
    }


type alias UnaryParam =
    { value : Int }


type alias Modes =
    { left : ParamMode
    , right : ParamMode
    , target : ParamMode
    }


type Instruction
    = Add TrinaryParams
    | Multiply TrinaryParams
    | Store UnaryParam
    | Output UnaryParam
    | JumpIfTrue BinaryParams
    | JumpIfFalse BinaryParams
    | LessThan TrinaryParams
    | Equal TrinaryParams
    | Halt
    | Error


type ParamMode
    = PositionMode
    | ImmediateMode


execute : State -> State
execute state =
    let
        instruction =
            nextInstruction state
    in
    case instruction of
        Add { left, right, target } ->
            execute
                { state
                    | state = setAt target (left + right) state.state
                    , instructionPointer = state.instructionPointer + 4
                    , log = instruction :: state.log
                }

        Multiply { left, right, target } ->
            execute
                { state
                    | state =
                        setAt target (left * right) state.state
                    , instructionPointer = state.instructionPointer + 4
                    , log = instruction :: state.log
                }

        Store { value } ->
            let
                input =
                    List.head state.inputs

                inputs_ =
                    List.drop 1 state.inputs
            in
            case input of
                Just v ->
                    execute
                        { state
                            | state =
                                setAt value v state.state
                            , inputs = inputs_
                            , instructionPointer = state.instructionPointer + 2
                            , log = instruction :: state.log
                        }

                Nothing ->
                    { state
                        | errors = "exhausted inputs" :: state.errors
                    }

        JumpIfTrue { left, right } ->
            case left of
                0 ->
                    execute
                        { state
                            | instructionPointer = state.instructionPointer + 3
                            , log = instruction :: state.log
                        }

                _ ->
                    execute
                        { state
                            | instructionPointer = right
                            , log = instruction :: state.log
                        }

        JumpIfFalse { left, right } ->
            case left of
                0 ->
                    execute
                        { state
                            | instructionPointer = right
                            , log = instruction :: state.log
                        }

                _ ->
                    execute
                        { state
                            | instructionPointer = state.instructionPointer + 3
                            , log = instruction :: state.log
                        }

        LessThan { left, right, target } ->
            if left < right then
                execute
                    { state
                        | state = setAt target 1 state.state
                        , instructionPointer = state.instructionPointer + 4
                        , log = instruction :: state.log
                    }

            else
                execute
                    { state
                        | state = setAt target 0 state.state
                        , instructionPointer = state.instructionPointer + 4
                        , log = instruction :: state.log
                    }

        Equal { left, right, target } ->
            if left == right then
                execute
                    { state
                        | state = setAt target 1 state.state
                        , instructionPointer = state.instructionPointer + 4
                        , log = instruction :: state.log
                    }

            else
                execute
                    { state
                        | state = setAt target 0 state.state
                        , instructionPointer = state.instructionPointer + 4
                        , log = instruction :: state.log
                    }

        Output { value } ->
            execute
                { state
                    | outputs = value :: state.outputs
                    , instructionPointer = state.instructionPointer + 2
                    , log = instruction :: state.log
                }

        Halt ->
            state

        Error ->
            { state
                | errors = "could not read instruction" :: state.errors
            }


valueAt : State -> Int -> Int
valueAt state address =
    getAt address state.state
        |> Maybe.withDefault -999999999


paramAt : State -> Int -> Int
paramAt state offset =
    valueAt state (state.instructionPointer + offset)


nextInstruction : State -> Instruction
nextInstruction state =
    let
        input =
            paramAt state 0

        opcode =
            remainderBy 100 input

        modes =
            readModes input
    in
    readInstruction state opcode modes


readModes : Int -> Modes
readModes input =
    { left = readMode (input // 100 |> remainderBy 10)
    , right = readMode (input // 1000 |> remainderBy 10)
    , target = readMode (input // 10000 |> remainderBy 10)
    }


readMode : Int -> ParamMode
readMode input =
    case input of
        0 ->
            PositionMode

        _ ->
            ImmediateMode


readInstruction : State -> Int -> Modes -> Instruction
readInstruction state opcode modes =
    case opcode of
        1 ->
            Add <| readTrinary state modes

        2 ->
            Multiply <| readTrinary state modes

        3 ->
            Store <| readUnaryImmediate state modes.left

        4 ->
            Output <| readUnary state modes.left

        5 ->
            JumpIfTrue <| readBinary state modes

        6 ->
            JumpIfFalse <| readBinary state modes

        7 ->
            LessThan <| readTrinary state modes

        8 ->
            Equal <| readTrinary state modes

        99 ->
            Halt

        _ ->
            Error


readTrinary : State -> Modes -> TrinaryParams
readTrinary state modes =
    let
        leftParam =
            paramAt state 1

        rightParam =
            paramAt state 2

        targetValue =
            paramAt state 3

        leftValue =
            case modes.left of
                ImmediateMode ->
                    leftParam

                PositionMode ->
                    valueAt state leftParam

        rightValue =
            case modes.right of
                ImmediateMode ->
                    rightParam

                PositionMode ->
                    valueAt state rightParam
    in
    { left = leftValue
    , right = rightValue
    , target = targetValue
    }


readBinary : State -> Modes -> BinaryParams
readBinary state modes =
    let
        leftParam =
            paramAt state 1

        rightParam =
            paramAt state 2

        leftValue =
            case modes.left of
                ImmediateMode ->
                    leftParam

                PositionMode ->
                    valueAt state leftParam

        rightValue =
            case modes.right of
                ImmediateMode ->
                    rightParam

                PositionMode ->
                    valueAt state rightParam
    in
    { left = leftValue
    , right = rightValue
    }


readUnaryImmediate : State -> ParamMode -> UnaryParam
readUnaryImmediate state mode =
    { value = paramAt state 1
    }


readUnary : State -> ParamMode -> UnaryParam
readUnary state mode =
    let
        leftParam =
            paramAt state 1

        leftValue =
            case mode of
                ImmediateMode ->
                    leftParam

                PositionMode ->
                    valueAt state leftParam
    in
    { value = leftValue
    }
