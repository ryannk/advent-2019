module Day2 exposing (main)

import Array exposing (Array)
import Html exposing (Html)



-- TYPES


type alias Program =
    Array Int


type alias Address =
    Int


type Operation
    = Instruction Instruction
    | Halt


type Instruction
    = Add Int Int Address
    | Multiply Int Int Address


type alias ParseOperationOutput =
    { nextLocation : Address
    , operation : Operation
    }


type Error
    = OutOfBounds Int
    | InvalidOperation Int


nextOperation : Address -> Program -> Result Error ParseOperationOutput
nextOperation pc program =
    readRegister pc program
        |> Result.andThen (parseOperation pc program)


readRegister : Int -> Program -> Result Error Int
readRegister location program =
    Array.get location program
        |> Result.fromMaybe (OutOfBounds location)


parse3Operation :
    (Int -> Int -> Address -> Instruction)
    -> Address
    -> Program
    -> Result Error ParseOperationOutput
parse3Operation fn pc program =
    Result.map3 fn
        (readRegister (pc + 1) program
            |> Result.andThen (\address -> readRegister address program)
        )
        (readRegister (pc + 2) program
            |> Result.andThen (\address -> readRegister address program)
        )
        (readRegister (pc + 3) program)
        |> Result.map Instruction
        |> Result.map (ParseOperationOutput (pc + 4))


parseOperation :
    Address
    -> Program
    -> Int
    -> Result Error ParseOperationOutput
parseOperation pc program operationCode =
    case operationCode of
        1 ->
            parse3Operation Add pc program

        2 ->
            parse3Operation Multiply pc program

        99 ->
            ParseOperationOutput -1 Halt
                |> Ok

        _ ->
            InvalidOperation operationCode
                |> Err



-- CODE


parseInput : String -> Program
parseInput str =
    String.split "," str
        |> List.filterMap String.toInt
        |> Array.fromList


programInput : String
programInput =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,5,19,23,1,23,5,27,2,27,10,31,1,5,31,35,2,35,6,39,1,6,39,43,2,13,43,47,2,9,47,51,1,6,51,55,1,55,9,59,2,6,59,63,1,5,63,67,2,67,13,71,1,9,71,75,1,75,9,79,2,79,10,83,1,6,83,87,1,5,87,91,1,6,91,95,1,95,13,99,1,10,99,103,2,6,103,107,1,107,5,111,1,111,13,115,1,115,13,119,1,13,119,123,2,123,13,127,1,127,6,131,1,131,9,135,1,5,135,139,2,139,6,143,2,6,143,147,1,5,147,151,1,151,2,155,1,9,155,0,99,2,14,0,0"


modifyInputs : { noun : Int, verb : Int } -> Program -> Program
modifyInputs { noun, verb } program =
    program
        |> Array.set 1 noun
        |> Array.set 2 verb



-- verb


main : Html msg
main =
    List.range 0 99
        |> List.concatMap
            (\noun ->
                List.range 0 99
                    |> List.map (\verb -> { noun = noun, verb = verb })
            )
        |> List.filter (\input -> matches input (parseInput programInput))
        |> Debug.toString
        |> Html.text


matches : { noun : Int, verb : Int } -> Program -> Bool
matches inputs program =
    program
        |> modifyInputs inputs
        |> run 0
        |> Result.andThen (readRegister 0)
        |> Result.map (\value -> value == 19690720)
        |> Result.withDefault False


run : Address -> Program -> Result Error Program
run currentAddress program =
    nextOperation currentAddress program
        |> Result.andThen
            (\{ nextLocation, operation } ->
                case operation of
                    Instruction instruction ->
                        update instruction program
                            |> run nextLocation

                    Halt ->
                        Ok program
            )


update : Instruction -> Program -> Program
update operation program =
    case operation of
        Add x y address ->
            Array.set address (x + y) program

        Multiply x y address ->
            Array.set address (x * y) program



-- TESTS


testMain =
    tests
        |> List.map
            (\test ->
                { input = parseInput test.input
                , output = Ok (parseInput test.output)
                }
            )
        |> List.map (runTest (run 0))
        |> Debug.toString
        |> Html.text


tests =
    [ { input = "1,0,0,0,99", output = "2,0,0,0,99" }
    , { input = "2,3,0,2,99", output = "2,3,0,6,99" }
    , { input = "2,4,4,5,99,0", output = "2,4,4,5,99,9801" }
    , { input = "1,1,1,4,99,5,6,0,99", output = "30,1,1,4,2,5,6,0,99" }
    ]


runTest fn test =
    if fn test.input == test.output then
        "Hooray for " ++ Debug.toString test.input

    else
        "Expected " ++ Debug.toString test.output ++ ", received " ++ Debug.toString (fn test.input)
