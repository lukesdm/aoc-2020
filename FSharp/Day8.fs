module Day8

open Expecto

type Instruction = | Nop

/// Runs the given instructions, returning the final value of the instruction counter
let run (instructions: Instruction []): int =
    let mutable instructionPointer = 0

    while instructionPointer < instructions.Length do
        let instruction = instructions.[instructionPointer]

        match instruction with
        | Nop -> instructionPointer <- instructionPointer + 1

    instructionPointer

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

            let instructionPointer = run instructions

            Expect.equal instructionPointer 1 ""
          } ]
