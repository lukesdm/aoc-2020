// https://adventofcode.com/2020/day/14
module Day14

open System.Text.RegularExpressions
open Expecto

/// Address-Value pair ("both 36-bit unsigned integers")
type Instruction = int64 * int64

/// Sparse array of Address-Value pairs
type Memory = Map<int64, int64>

type Mask = string

let apply (mask: Mask) (value: int64): int64 =
    // Build separate masks for 0s and 1s, and then apply using bitwise ops
    let m0 =
        System.Convert.ToInt64(mask.Replace("X", "1"), 2)

    let m1 =
        System.Convert.ToInt64(mask.Replace("X", "0"), 2)

    (value ||| m1) &&& m0

let instRegex = new Regex("mem\[(\d+)\] = (\d+)")

let parseInst (text: string): Instruction =
    let groups = instRegex.Match(text).Groups
    int64 groups.[1].Value, int64 groups.[2].Value

let parse (input: string): seq<Mask * Instruction []> =
    // Group instructions by mask, from flat file
    let lines = input.Split('\n')

    let masks =
        lines
        |> Seq.indexed
        |> Seq.choose (fun (idx, line) -> if line.StartsWith("mask = ") then Some(idx, line.[7..]) else None)

    let (sequence, _) =
        Seq.mapFoldBack
            (fun (idxCurr, mask) idxEnd -> (mask, Array.map parseInst lines.[idxCurr + 1..idxEnd - 1]), idxCurr)
            masks
            lines.Length

    sequence

let part1 input =
    let insts =
        parse input
        |> Seq.collect (fun (mask, is) -> Seq.map (fun i -> (mask, i)) is)

    let finalMem =
        insts
        |> Seq.fold (fun mem (mask, (addr, value)) -> Map.add addr (apply mask value) mem) Map.empty

    finalMem |> Seq.sumBy (fun kvp -> kvp.Value)

let solve input = part1 input

let example1 = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"

// Sample from actual input
let example2 = "mask = 110000011XX0000X101000X10X01XX001011
mem[49397] = 468472
mem[50029] = 23224119
mem[39033] = 191252712
mem[37738] = 25669
mem[45831] = 238647542
mem[55749] = 1020
mem[29592] = 57996
mask = 100X10XXX101011X10X0110111X01X0X0010
mem[10526] = 1843
mem[2144] = 177500
mem[33967] = 5833292
mem[58979] = 25707732
mask = 100010X011XX00X11011010011101100XXX1
mem[1729] = 1042
mem[30433] = 366890
mem[7726] = 2862
mem[19747] = 52273994
mask = 11001X0011010110X01X011X001X0XX01010
mem[40528] = 32637378
mem[16008] = 30888145"

let tests =
    testList
        "Day 14"
        [ test "Can apply mask to value" {
            let value = 11L
            let mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            let result_expected = 73L

            let result_actual = apply mask value

            Expect.equal result_actual result_expected ""
          }
          test "Can parse input" {
              let input = example2.Replace("\r\n", "\n")

              let result = parse input

              printfn "%A" result

              Expect.isTrue true ""
          }
          test "Example 1- Can solve - sum" {
              let input = example1.Replace("\r\n", "\n")
              // "In the above example, only two values in memory are not zero - 101 (at address 7) and 64 (at address 8) - producing a sum of 165."
              let expected = 165L

              let actual = part1 input

              Expect.equal actual expected ""
          } ]
