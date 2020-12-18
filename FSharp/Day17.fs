module Day17

open Expecto

/// Cell position: X, Y, Z
type Cell = int * int * int

/// Collection of active cells
type Cells = Set<Cell>

let toXyz row col = (col, row, 0)

/// Parses the input string into a collection of active cells
let parse (input: string): Cells =
    let lines =
        input.Replace("\r\n", "\n").Split("\n")
        |> Array.indexed

    seq {
        for (row, line) in lines do
            for col in 0 .. lines.Length - 1 do
                if line.[col] = '#' then yield toXyz row col
    }
    |> Set.ofSeq

let tests =
    testList
        "Day11"
        [ test "Can Parse" {
            // X = Left to Right
            // Y = Bottom to Top
            // Z = Far to Near
            let input = ".#.
..#
###"

            let activeCells_expected =
                Set.ofList [ (1, 0, 0)
                             (2, 1, 0)
                             (0, 2, 0)
                             (1, 2, 0)
                             (2, 2, 0) ]

            let activeCells_actual = parse input

            Expect.equal activeCells_actual activeCells_expected ""
          } ]
