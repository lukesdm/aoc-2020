/// https://adventofcode.com/2020/day/3
module Day3

open Expecto

type Grid = int [,]
type Vec = { X: int; Y: int }

/// Generates a 2D array of 0s (open space) and 1s (trees) from the supplied text lines, where a '.' is open space, and a '#' is a tree.
let parse (lines: string []): Grid =
    let height = lines.Length
    let width = lines.[0].Length
    let result = Array2D.zeroCreate height width

    for row in 0 .. height - 1 do
        for col in 0 .. width - 1 do
            result.[row, col] <- match lines.[row].Chars(col) with
                                 | '#' -> 1
                                 | _ -> 0

    result

let countTrees (input: Grid) (velocity: Vec): int =
    let height = input.GetLength(0)
    let width = input.GetLength(1)
    let mutable col = 0
    let mutable row = 0
    let mutable treeCount = 0

    while row < height do
        treeCount <- treeCount + input.[row, col % width]
        col <- col + velocity.X
        row <- row + velocity.Y

    treeCount

let part1 (input: string []): int =
    countTrees (parse input) { X = 3; Y = 1 }

let solve input = (part1 input, 0)

let tests =
    testList
        "Day 3"
        [ test "Can parse input" {
            // Not square, so we ensure we get rows and columns the right way round
            let input = [| ".#"; "#."; "##" |]

            let expected = array2D [ [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ]

            Expect.equal (parse input) expected ""
          }
          test "Example - Counts trees correctly" {
              let input =
                  [| "..##......."
                     "#...#...#.."
                     ".#....#..#."
                     "..#.#...#.#"
                     ".#...##..#."
                     "..#.##....."
                     ".#.#.#....#"
                     ".#........#"
                     "#.##...#..."
                     "#...##....#"
                     ".#..#...#.#" |]
                  |> parse

              let vel: Vec = { X = 3; Y = 1 }
              let tc_expected = 7

              let tc_actual = countTrees input vel

              Expect.equal tc_actual tc_expected ""
          } ]
