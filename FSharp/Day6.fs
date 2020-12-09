// https://adventofcode.com/2020/day/6
module Day6

open Expecto

/// "In this example, the sum of these counts is 3 + 3 + 3 + 1 + 1 = 11"
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

let solve input = part1 input

let tests =
    testList
        "Day 6"
        [ test "Example 1 - Can parse into answer groups" {
            let input = example1.Replace("\r\n", "\n")
            let answerGroupCount_Expected = 5

            let answerGroupCount_Actual = Seq.length (parseAnswerGroups input)
            Expect.equal answerGroupCount_Actual answerGroupCount_Expected ""
          }
          test "Example 1 - Get answer counts for groups" {
              // The form asks a series of 26 yes-or-no questions marked a through z
              // 'identify the questions for which anyone in your group answers "yes"'
              let input = example1.Replace("\r\n", "\n")

              // Answer counts for groups, first to last
              let expected = [ 3; 3; 3; 1; 1 ]

              let actual = calcAllAnswerCounts input

              Expect.equal (List.ofSeq actual) expected ""
          }
          test "Example 1 - Calc sum of counts" {
              let input = example1.Replace("\r\n", "\n")
              let expected = 11

              let actual = (part1 input)

              Expect.equal actual expected ""
          } ]
