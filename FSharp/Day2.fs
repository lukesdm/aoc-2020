/// https://adventofcode.com/2020/day/2
module Day2

open Expecto
open System.Text.RegularExpressions

let regex = new Regex("^(\d+)-(\d+) (.): (.+)")

// *** PART 1 ***
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

// *** PART 2 ***
type Password2 =
    { Pos1: int
      Pos2: int
      Char: char
      Value: string }

let parseLine2 (inputLine: string): Password2 =
    let m = regex.Match inputLine

    { Pos1 = int m.Groups.[1].Value
      Pos2 = int m.Groups.[2].Value
      Char = char m.Groups.[3].Value
      Value = m.Groups.[4].Value }

let xor (a: bool) (b: bool): bool = (a || b) && not (a && b)

/// "Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character,
/// and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter."
let validatePassword2 (input: Password2): bool =
    let index1 = input.Pos1 - 1
    let index2 = input.Pos2 - 1
    xor (input.Value.[index1] = input.Char) (input.Value.[index2] = input.Char)

let part2 (input: seq<string>): int =
    input
    |> Seq.map parseLine2
    |> Seq.filter validatePassword2
    |> Seq.length

// *** ... ***
let solve input = (part1 input, part2 input)

let tests =
    testList
        "Day 2"
        [ test "Part 1 - Can parse line" {
            let inputLine = "1-3 a: abcde"

            let expected =
                { Min = 1
                  Max = 3
                  Char = "a"
                  Value = "abcde" }

            Expect.equal (parseLine inputLine) expected ""
          }
          test "Part 1 - Can validate password" {
              let input = parseLine "1-3 a: abcde"

              Expect.isTrue (validatePassword input) ""
          }
          test "Part 1 - Example 1 - gets correct number of valid passwords" {
              let input =
                  [ "1-3 a: abcde"
                    "1-3 b: cdefg"
                    "2-9 c: ccccccccc" ]

              // "In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1.
              // The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies."
              let correct_expected = 2

              let correct_actual = part1 input

              Expect.equal correct_actual correct_expected ""
          }
          test "Part 2 - Can parse line" {
              let inputLine = "1-3 a: abcde"

              let expected: Password2 =
                  { Pos1 = 1
                    Pos2 = 3
                    Char = 'a'
                    Value = "abcde" }

              Expect.equal (parseLine2 inputLine) expected ""
          }
          test "Part 2 - Can validate valid password" {
              let input = parseLine2 "1-3 a: abcde"

              Expect.isTrue (validatePassword2 input) ""
          }
          test "Part 2 - Cannot validate invalid password - no occurrences" {
              let input = parseLine2 "1-3 b: cdefg"

              Expect.isFalse (validatePassword2 input) ""
          }
          test "Part 2 - Cannot validate invalid password - multiple occurrences" {
              let input = parseLine2 "2-9 c: ccccccccc"

              Expect.isFalse (validatePassword2 input) ""
          } ]
