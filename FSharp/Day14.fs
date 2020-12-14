module Day14

open Expecto

let apply (mask: string) (value: int64): int64 =
    // Build separate masks for 0s and 1s, and then apply using bitwise ops
    let m0 =
        System.Convert.ToInt64(mask.Replace("X", "1"), 2)

    let m1 =
        System.Convert.ToInt64(mask.Replace("X", "0"), 2)

    (value ||| m1) &&& m0

let example = "mask = 110000011XX0000X101000X10X01XX001011A
mem[49397] = 468472
mem[50029] = 23224119
mem[39033] = 191252712
mem[37738] = 25669
mem[45831] = 238647542
mem[55749] = 1020
mem[29592] = 57996
mask = 100X10XXX101011X10X0110111X01X0X0010B
mem[10526] = 1843
mem[2144] = 177500
mem[33967] = 5833292
mem[58979] = 25707732
mask = 100010X011XX00X11011010011101100XXX1C
mem[1729] = 1042
mem[30433] = 366890
mem[7726] = 2862
mem[19747] = 52273994
mask = 11001X0011010110X01X011X001X0XX01010D
mem[40528] = 32637378
mem[16008] = 30888145"

let parse (input: string) =
    let lines = input.Split('\n')

    let masks =
        lines
        |> Seq.indexed
        |> Seq.choose (fun (idx, line) -> if line.StartsWith("mask = ") then Some(idx, line.[7..]) else None)

    //let idx, mask = Seq.last masks
    //let lastMask = mask, idx + 1, lines.Length - 1
    //let maskRanges =
    //    Seq.rev masks
    //    |> Seq.pairwise
    //    |> Seq.map (fun ((idxNext, _), (idxCurr, maskCurr)) -> (maskCurr, idxCurr + 1, idxNext - 1))
    //    |> Seq.append [ lastMask ]
    //    |> Seq.rev

    let (maskRanges, _) =
        Seq.mapFoldBack
            (fun (idxCurr, mask) idxEnd -> (mask, lines.[idxCurr + 1..idxEnd - 1]), idxCurr)
            masks
            lines.Length

    maskRanges






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
              let input = example.Replace("\r\n", "\n")

              let result = parse input

              printfn "%A" result

              Expect.isTrue true
          } ]
