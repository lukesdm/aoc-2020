module Day8

open Expecto

type Instruction =
    | Nop
    | Jmp of int
    | Acc of int

type State =
    { mutable InstructionPointer: int
      mutable Accumulator: int }

/// Runs the given instructions, returning the final state
let run (instructions: Instruction []): State =
    let state =
        { InstructionPointer = 0
          Accumulator = 0 }

    while state.InstructionPointer < instructions.Length do
        let instruction = instructions.[state.InstructionPointer]

        match instruction with
        | Nop -> state.InstructionPointer <- state.InstructionPointer + 1
        | Jmp n -> state.InstructionPointer <- state.InstructionPointer + n
        | Acc n ->
            state.Accumulator <- state.Accumulator + n
            state.InstructionPointer <- state.InstructionPointer + 1

    state

let parseLine (line: string): Instruction =
    let instructionToken = line.[0..2]
    let argumentToken = line.[4..]
    match instructionToken with
    | "nop" -> Nop
    | "jmp" -> Jmp (int argumentToken)
    | "acc" -> Acc (int argumentToken)
    | _ -> failwith "Unexpected input"
    
let parse (input: string) : Instruction[] =
    input.Replace("\r\n", "\n").Split('\n') |> Array.map parseLine
    
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

            let state = run instructions

            Expect.equal state.InstructionPointer 1 ""
          }
          test "jmp" {
              let instructions = [| Jmp 4 |]

              let state = run instructions

              Expect.equal state.InstructionPointer 4 ""
          }
          test "acc" {
              let instructions = [| Acc 2; Acc 7 |]

              let state = run instructions

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
          } ]
