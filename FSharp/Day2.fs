/// https://adventofcode.com/2020/day/2
module Day2

open Expecto
open System.Text.RegularExpressions

let regex = new Regex("^(\d+)-(\d+) (.): (.+)")

type Password =
    { Min: int
      Max: int
      Char: string
      Value: string }

let parseLine (inputLine: string): Password =
    let m = regex.Match inputLine

    { Min = int m.Groups.[1].Value
      Max = int m.Groups.[2].Value
      Char = m.Groups.[3].Value
      Value = m.Groups.[4].Value }


let validatePassword (input: Password): bool =
    let matches = Regex.Matches(input.Value, input.Char)
    let count = Seq.length matches
    count >= input.Min && count <= input.Max

/// "How many passwords are valid according to their policies?"
let part1 (input: seq<string>): int =
    input
    |> Seq.map parseLine
    |> Seq.filter validatePassword
    |> Seq.length

/// TODO
let part2 input = 0

let solve input = (part1 input, part2 input)

let tests =
    testList
        "Day 2"
        [ test "Can parse line" {
            let inputLine = "1-3 a: abcde"

            let expected =
                { Min = 1
                  Max = 3
                  Char = "a"
                  Value = "abcde" }

            Expect.equal (parseLine inputLine) expected ""
          }
          test "Can validate password" {
              let input = parseLine "1-3 a: abcde"

              Expect.isTrue (validatePassword input) ""
          }
          test "Part 1 - Example 1 - gets correct number of valid passwords" {
              let input =
                  [ "1-3 a: abcde"
                    "1-3 b: cdefg"
                    "2-9 c: ccccccccc" ]

              // In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
              let correct_expected = 2

              let correct_actual = part1 input

              Expect.equal correct_actual correct_expected ""
          } ]
