// https://adventofcode.com/2020/day/9
module Day9

open Expecto

let parse (input: string): int64 [] =
    input.Replace("\r\n", "\n").Split('\n')
    |> Array.map System.Int64.Parse


let isValid i candidates =
    candidates
    |> Seq.exists (fun (n1, n2) -> i = n1 + n2)

/// Gets non-matching pairs of a sequence. Assumes no identical items in the sequence, as per:
/// "The two numbers will have different values, and there might be more than one such pair"
let pairs inSeq =
    seq {
        for l in inSeq do
            for r in inSeq do
                if l <> r then l, r
    }

let findInvalid (windowSize: int) (input: int64 []): int64 =
    let candidates =
        input
        |> Seq.windowed windowSize
        |> Seq.map (fun win -> pairs win)

    let data = input |> Seq.skip windowSize

    let (firstInvalid, _) =
        Seq.map2 (fun d c -> d, isValid d c) data candidates
        |> Seq.find (fun (_, valid) -> not valid)

    firstInvalid

let part1 (input: string) = parse input |> findInvalid 25

let solve (input: string) = part1 input

let example1 = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"

let tests =
    testList
        "Day 9"
        [ test "Can parse" {
            let input = "35
            20
            15
            25"
            let expected = [| 35L; 20L; 15L; 25L |]

            let actual = parse input
            Expect.equal actual expected ""
          }
          test "Example 1 - Can Validate" {
              // "In this example, after the 5-number preamble, almost every number is the sum of two of the previous 5 numbers; the only number that does not follow this rule is 127."
              let input = parse example1
              let expected = 127L

              let actual = findInvalid 5 input

              Expect.equal actual expected ""
          } ]
