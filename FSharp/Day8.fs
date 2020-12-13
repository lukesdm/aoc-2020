// https://adventofcode.com/2020/day/8
module Day8

open Expecto

type Instruction =
    | Nop of int
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

let flip (instruction: Instruction): Instruction =
    match instruction with
    | Nop n -> Jmp n
    | Jmp n -> Nop n
    | _ -> failwith "Unexpected instruction"

let rec run (instructions: Instruction [])
            (state: State)
            (runCounts: int [])
            (buildCandidates: bool)
            (candidates: Candidates)
            : State * Candidates =

    if state.InstructionPointer > instructions.Length - 1 then
        { state with Status = Finished }, candidates
    else if runCounts.[state.InstructionPointer] > 0 then
        { state with Status = InfiniteLoop }, candidates
    else
        let (instruction, patched) =
            if List.tryHead candidates = Some state.InstructionPointer
            then (flip instructions.[state.InstructionPointer], true)
            else (instructions.[state.InstructionPointer], false)
        runCounts.[state.InstructionPointer] <- runCounts.[state.InstructionPointer] + 1

        let result =
            match instruction with
            | Nop _ ->
                { state with
                      InstructionPointer = state.InstructionPointer + 1 },
                (if buildCandidates
                 then state.InstructionPointer :: candidates
                 else (if patched then candidates.Tail else candidates))
            | Jmp n ->
                { state with
                      InstructionPointer = state.InstructionPointer + n },
                (if buildCandidates
                 then state.InstructionPointer :: candidates
                 else (if patched then candidates.Tail else candidates))
            | Acc n ->
                { state with
                      Accumulator = state.Accumulator + n
                      InstructionPointer = state.InstructionPointer + 1 },
                candidates

        let newState, newCandidates = result
        run instructions newState runCounts buildCandidates newCandidates

let run0 (instructions: Instruction []): State =
    let state, _ =
        run
            instructions
            { InstructionPointer = 0
              Accumulator = 0
              Status = Running }
            (Array.zeroCreate instructions.Length)
            true
            []

    state

let rec trial (state: State) (instructions: Instruction []) (candidates: Candidates): State =
    if state.Status = Finished then
        state
    else
        let newState, newCandidates =
            run
                instructions
                { InstructionPointer = 0
                  Accumulator = 0
                  Status = Running }
                (Array.zeroCreate instructions.Length)
                false
                candidates

        trial newState instructions newCandidates


let run2 (instructions: Instruction []): State =
    let state, candidates =
        run
            instructions
            { InstructionPointer = 0
              Accumulator = 0
              Status = Running }
            (Array.zeroCreate instructions.Length)
            true
            []

    trial state instructions candidates


let parseLine (line: string): Instruction =
    let instructionToken = line.[0..2]
    let argumentToken = line.[4..]

    match instructionToken with
    | "nop" -> Nop(int argumentToken)
    | "jmp" -> Jmp(int argumentToken)
    | "acc" -> Acc(int argumentToken)
    | _ -> failwith "Unexpected input"

let parse (input: string): Instruction [] =
    input.Replace("\r\n", "\n").Split('\n')
    |> Array.map parseLine

let part1 (input: string): int =
    let finalState = input |> parse |> run0
    finalState.Accumulator

let part2 (input: string): int =
    let finalState = input |> parse |> run2
    finalState.Accumulator

let solve input = part1 input, part2 input

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
            let instructions = [| Nop 0 |]

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
                  [| Nop 0
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
          }
          test "Part 2 - Example 1 - Can patch program successfully" {
              let instructions = parse example1

              let state = run2 instructions

              Expect.equal state.Status Finished ""
          } ]
