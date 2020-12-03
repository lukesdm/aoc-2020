module Day3

open Expecto

/// Generates a 2D array of 0s (open space) and 1s (trees) from the supplied text lines, where a '.' is open space, and a '#' is a tree.
let parse (lines: string []): int [,] =
    let height = lines.Length
    let width = lines.[0].Length
    let result = Array2D.zeroCreate height width

    for row in 0 .. height - 1 do
        for col in 0 .. width - 1 do
            result.[row, col] <- match lines.[row].Chars(col) with
                                 | '#' -> 1
                                 | _ -> 0

    result

let exampleInput =
    [ "..##......."
      "#...#...#.."
      ".#....#..#."
      "..#.#...#.#"
      ".#...##..#."
      "..#.##....."
      ".#.#.#....#"
      ".#........#"
      "#.##...#..."
      "#...##....#"
      ".#..#...#.#" ]

let tests =
    testList
        "Day 3"
        [ test "Can parse input" {
            // Not square, so we ensure we get rows and columns the right way round
            let input = [| ".#"; "#."; "##" |]

            let expected = array2D [ [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ] ]

            Expect.equal (parse input) expected ""
          } ]
