/// https://adventofcode.com/2020/day/1
module Day1

open Expecto

/// "Find the two entries that sum to 2020 and then multiply those two numbers together"
let part1 input =
    Seq.allPairs input input
    |> Seq.filter (fun (l, r) -> l + r = 2020)
    |> Seq.map (fun (l, r) -> l * r)
    |> Seq.head

// TomasP: https://stackoverflow.com/a/4495708
let rec combinations acc size set =
    seq {
        match size, set with
        | n, x :: xs ->
            if n > 0
            then yield! combinations (x :: acc) (n - 1) xs

            if n >= 0 then yield! combinations acc n xs
        | 0, [] -> yield acc
        | _, [] -> ()
    }


/// Like part 1, but for 3 numbers.
let part2 input =
    List.ofSeq input
    |> combinations [] 3
    |> Seq.filter (fun trio -> List.sum trio = 2020)
    |> Seq.head
    |> List.reduce (fun acc el -> acc * el)

let solve input = (part1 input, part2 input)

let tests =
    testList
        "Day 1"
        [ test "Part 1 - example 1" {
            let input = [ 1721; 979; 366; 299; 675; 1456 ]

            let (part1_actual, _) = solve input
            Expect.equal part1_actual 514579 ""
          }

          test "Part 2 - example 1" {
              let input = [ 1721; 979; 366; 299; 675; 1456 ]

              let (_, part2_actual) = solve input
              Expect.equal part2_actual 241861950 ""
          } ]
