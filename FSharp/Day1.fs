/// https://adventofcode.com/2020/day/1
module Day1

open Expecto

// "find the two entries that sum to 2020 and then multiply those two numbers together"
let part1 input =
    Seq.allPairs input input
    |> Seq.filter (fun (l, r) -> l + r = 2020)
    |> Seq.map (fun (l, r) -> l * r)
    |> Seq.head

// TODO:
let part2 _ = 24

let solve input = (part1 input, part2 input)

let tests =
    testList
        "Part 1"
        [ test "example 1" {
            let input = [ 1721; 979; 366; 299; 675; 1456 ]

            let (part1_actual, _) = solve input
            Expect.equal part1_actual 514579 ""
          } ]
