/// https://adventofcode.com/2020/day/2
module Day2

open Expecto
open System.Text.RegularExpressions

let regex = new Regex("^(\d+)-(\d+) (.): (.+)")

let parseLine inputLine =
    let m = regex.Match inputLine

    {| Min = int m.Groups.[1].Value
       Max = int m.Groups.[2].Value
       Char = m.Groups.[3].Value
       Password = m.Groups.[4].Value |}

/// "How many passwords are valid according to their policies?"
let part1 input = input

/// TODO
let part2 input = input

let solve input = (part1 input, part2 input)

let tests =
    testList
        "Day 2"
        [ test "Can parse line" {
            let inputLine = "1-3 a: abcde"

            let expected =
                {| Min = 1
                   Max = 3
                   Char = "a"
                   Password = "abcde" |}

            Expect.equal (parseLine inputLine) expected ""
          } ]
