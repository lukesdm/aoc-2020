// https://adventofcode.com/2020/day/6
module Day6

open Expecto

/// Part 1 - "In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11"
///
/// Part 2 - "In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6"
let example1 = "abc

a
b
c

ab
ac

a
a
a
a

b"

let parseAnswerGroups (input: string): seq<string []> =
    input.Split("\n\n")
    |> Seq.map (fun s -> s.Split("\n"))

let buildSet (acc: Set<char>) (input: string): Set<char> =
    input
    |> Seq.fold (fun set cha -> Set.add cha set) acc

let calcGroupAnswerCounts (membersAnswers: string []): int =
    let init: Set<char> = Set.empty

    membersAnswers
    |> Array.fold buildSet init
    |> Set.count

let calcAllAnswerCounts (input: string): seq<int> =
    input
    |> parseAnswerGroups
    |> Seq.map calcGroupAnswerCounts

let part1 input = Seq.sum (calcAllAnswerCounts input)


// ***PART 2***
let calcGroupAnswerCounts2 (membersAnswers: string []): int =
    let sets =
        membersAnswers
        |> Array.map (fun str -> Set.ofArray (str.ToCharArray()))

    let is = sets |> Seq.reduce Set.intersect
    is.Count

let calcAllAnswerCounts2 (input: string): seq<int> =
    input
    |> parseAnswerGroups
    |> Seq.map calcGroupAnswerCounts2

let part2 (input: string): int = Seq.sum (calcAllAnswerCounts2 input)

// ***...***

let solve (input: string) = (part1 input, part2 input)

let tests =
    testList
        "Day 6"
        [ test "Example 1 - Can parse into answer groups" {
            let input = example1.Replace("\r\n", "\n")
            let answerGroupCount_Expected = 5

            let answerGroupCount_Actual = Seq.length (parseAnswerGroups input)
            Expect.equal answerGroupCount_Actual answerGroupCount_Expected ""
          }
          test "Part 1 - Example 1 - Get answer counts for groups" {
              // The form asks a series of 26 yes-or-no questions marked a through z
              // 'identify the questions for which anyone in your group answers "yes"'
              let input = example1.Replace("\r\n", "\n")

              // Answer counts for groups, first to last
              let expected = [ 3; 3; 3; 1; 1 ]

              let actual = calcAllAnswerCounts input

              Expect.equal (List.ofSeq actual) expected ""
          }
          test "Part 1 - Example 1 - Calc sum of counts" {
              let input = example1.Replace("\r\n", "\n")
              let expected = 11

              let actual = (part1 input)

              Expect.equal actual expected ""
          }
          test "Part 2 - Example 1 - Get answer counts for groups" {
              let input = example1.Replace("\r\n", "\n")
              let expected = [ 3; 0; 1; 1; 1 ]

              let actual = calcAllAnswerCounts2 input

              Expect.equal (List.ofSeq actual) expected ""
          }
          test "Part 2 - Example 1 - Calc sum of counts" {
              let input = example1.Replace("\r\n", "\n")
              let expected = 6

              let actual = (part2 input)

              Expect.equal actual expected ""
          } ]
