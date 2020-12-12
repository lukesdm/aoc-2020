// https://adventofcode.com/2020/day/8
module Day8

open Expecto

type Instruction =
    | Nop
    | Jmp of int
    | Acc of int

type Status =
    | Running
    | InfiniteLoop
    | Finished

type State =
    { mutable InstructionPointer: int
      mutable Accumulator: int
      mutable Status: Status }

/// Collection of indices of instructions to flip that might fix the infinite loop
type Candidates = list<int>

/// Runs the given instructions, returning the final state
//let run (instructions: Instruction []) (candidates: Candidates): (State * Candidates) =

//    // Flag for whether this is the first run, in which the list of candidates for patching are built
//    let buildCandidates = List.isEmpty candidates

let rec run (instructions: Instruction []) (state: State) (runCounts: int []) =
    if state.InstructionPointer > instructions.Length - 1 then
        { state with Status = Finished }
    else if runCounts.[state.InstructionPointer] > 0 then
        { state with Status = InfiniteLoop }
    else
        let instruction = instructions.[state.InstructionPointer]
        runCounts.[state.InstructionPointer] <- runCounts.[state.InstructionPointer] + 1

        let newState =
            match instruction with
            | Nop ->
                { state with
                      InstructionPointer = state.InstructionPointer + 1 }
            | Jmp n ->
                { state with
                      InstructionPointer = state.InstructionPointer + n }
            | Acc n ->
                { state with
                      Accumulator = state.Accumulator + n
                      InstructionPointer = state.InstructionPointer + 1 }

        run instructions newState runCounts

let run0 (instructions: Instruction []): State =
    run
        instructions
        { InstructionPointer = 0
          Accumulator = 0
          Status = Running }
        (Array.zeroCreate instructions.Length)

let parseLine (line: string): Instruction =
    let instructionToken = line.[0..2]
    let argumentToken = line.[4..]

    match instructionToken with
    | "nop" -> Nop
    | "jmp" -> Jmp(int argumentToken)
    | "acc" -> Acc(int argumentToken)
    | _ -> failwith "Unexpected input"

let parse (input: string): Instruction [] =
    input.Replace("\r\n", "\n").Split('\n')
    |> Array.map parseLine

let part1 (input: string): int =
    let finalState = input |> parse |> run0
    finalState.Accumulator

let solve input = part1 input

let example1 = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

let tests =
    testList
        "Day 8"
        [ test "nop" {
            let instructions = [| Nop |]

            let state = run0 instructions

            Expect.equal state.InstructionPointer 1 ""
          }
          test "jmp" {
              let instructions = [| Jmp 4 |]

              let state = run0 instructions

              Expect.equal state.InstructionPointer 4 ""
          }
          test "acc" {
              let instructions = [| Acc 2; Acc 7 |]

              let state = run0 instructions

              Expect.equal state.Accumulator 9 ""
          }
          test "Example 1 - Parse" {
              let expected =
                  [| Nop
                     Acc 1
                     Jmp 4
                     Acc 3
                     Jmp -3
                     Acc -99
                     Acc 1
                     Jmp -4
                     Acc 6 |]

              let actual = parse example1

              Expect.equal actual expected ""
          }
          test "Example 1 - Break before infinite loop" {
              // "Immediately before the program would run an instruction a second time, the value in the accumulator is 5."
              let acc_expected = 5
              let instructions = parse example1

              let state = run0 instructions
              let acc_actual = state.Accumulator

              Expect.equal acc_actual acc_expected ""
          } ]
