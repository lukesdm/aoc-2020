﻿// https://adventofcode.com/2020/day/14
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

              //printfn "%A" result

              Expect.isTrue true ""
          }
          test "Example 1- Can solve - sum" {
              let input = example1.Replace("\r\n", "\n")
              // "In the above example, only two values in memory are not zero - 101 (at address 7) and 64 (at address 8) - producing a sum of 165."
              let expected = 165L

              let actual = part1 input

              Expect.equal actual expected ""
          }
          
          /// Builds the 'X' address mask values for the given counter value i
          let rec xVals maskAcc n i (xPositions: int[]) =
            if n < 0 then
                maskAcc
            else
                // Calculate nth bit of the counter value i, shift it to the new position, and merge with the rest
                let nthBit = (i &&& (1L <<< n) >>> n )
                let next = nthBit <<< xPositions.[n] ||| maskAcc
                xVals next (n - 1) i xPositions
          
          test "Part 2 - WIP" {
              //let mask = "110000011XX0000X101000X10X01XX001011"
              //let addr0 = 6L
              let addr0 = 42L
              //          000000000000000000000000000000101010
              let mask = "000000000000000000000000000000X1001X"
              //          000000000000000000000000000000100001
              let xMask = mask |> String.map (fun c -> if c = 'X' then '1' else '0')
              let xMask = System.Convert.ToInt64(xMask, 2)
              //let mask ="X01X"
              //let addr0 = 0L
              //let mask = "X00X"

              let xPositions = mask |> Seq.rev |> Seq.indexed |> Seq.choose (fun (pos, c) -> if c = 'X' then Some(pos) else None) |> Array.ofSeq

              let xCount = xPositions.Length
              
              //let newBits = seq {0L .. 2L <<< xPositions.Length - 1} |> Seq.map (fun i -> System.Convert.ToString(i, 2).PadLeft(xCount, '0')) |> Array.ofSeq

              //let eg = Seq.head newBits
              
                


                
                

              //let addr0 = 49397L

              // "If the bitmask bit is 0, the corresponding memory address bit is unchanged.
              // If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1."
              let addr1 = addr0 ||| System.Convert.ToInt64 (mask.Replace ('X', '0'), 2)
              
              //// TODO: switch-out powns with shifts
              
              //let s = seq {
              //      for i in 0L..pown 2L (xCount - 1) do
              //          for xPos in xPositions do
              //              let xMask = pown 2L xPos
              //              yield (i <<< xPos) ||| xMask &&& addr1
              //    }

              //let mutable addr = addr1
              //let addresses = seq {
              //    for i in 0L..2L<<<(xCount - 1) do
              //      // TODO: Replace with reduce on xPositions
              //      let mutable xMask = 0L
              //      for xPos in xPositions do
              //          xMask <- (i <<< xPos) ||| xMask
              //      yield addr1 ||| xMask
              //  }

              //let addresses = seq {
              //  for i in 0L..2L<<<(xCount - 1) do
              //      // TODO: Replace with reduce on xPositions
              //      let mutable xMask = 0L
              //      let mutable n = 0
              //      for xPos in xPositions do
              //          //printfn $"nth bit of {i} is {i>>>n}"
              //          //xMask <- ((i>>>n) <<< (xPos)) ||| xMask
              //          //xMask <- ((i <<< (xPos)) &&& (1L <<< xPos)) ||| xMask
              //          xMask <- ((i &&& (1L <<< n) >>> n ) <<< xPos) ||| xMask
              //          n <- n + 1
              //          //xMask <- (i <<< xPos) ||| xMask
              //      yield addr1 ||| xMask
              //    }

              let addresses = seq {
                    for i in 0L..(2L<<<xCount-1) - 1L do
                        // TODO: Replace with reduce on xPositions
                        //let mutable xVals = 0L
                        //let mutable n = 0
                        
                        //yield addr1 ||| xMask 0L (xPositions.Length - 1) i xPositions
                        let xVals = xVals 0L (xPositions.Length - 1) i xPositions
                        //for xPos in xPositions do
                        //    //printfn $"nth bit of {i} is {i>>>n}"
                        //    //xMask <- ((i>>>n) <<< (xPos)) ||| xMask
                        //    //xMask <- ((i <<< (xPos)) &&& (1L <<< xPos)) ||| xMask
                        //    xVals <- ((i &&& (1L <<< n) >>> n ) <<< xPos) ||| xVals
                        //    n <- n + 1
                        //    xVals <- (i <<< xPos) ||| xVals
                        printfn "%s" (System.Convert.ToString(xVals, 2).PadLeft(8, '0'))
                        yield (addr1 &&& ~~~xMask ) ||| (xMask &&& xVals)
                    }

              printfn "%A" (Array.ofSeq addresses)
              
              Expect.isTrue true ""
          } ]
