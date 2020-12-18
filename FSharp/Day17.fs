/// https://adventofcode.com/2020/day/17
///
/// TL;DR: Game of Life 3D
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

let countNeighbors cells (x, y, z) =
    seq {
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                for dz in -1 .. 1 do
                    if (dx, dy, dz) <> (0, 0, 0) then yield (dx, dy, dz)
    }
    |> Seq.filter (fun (dx, dy, dz) -> Set.contains (x + dx, y + dy, z + dz) cells)
    |> Seq.length

let next (cells: Cells): Cells =
    seq {
        for cell in cells do
            yield cell
    }
    |> Set.ofSeq

let gen i0 =
    i0
    |> Seq.unfold
        (fun state ->
            let nxt = next state
            Some(nxt, nxt))

// Given initial state i0, runs the generator for n iterations.
let run i0 n = gen i0 |> Seq.item n

let tests =
    testList
        "Day17"
        [ test "Can Parse" {
            // X = Left to Right
            // Y = Top to Bottom
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
          }
          test "Can count active neighbors" {
              let state =
                  Set.ofList [ (0, 0, -1) // <-- neighbor
                               (2, 1, -1)
                               (1, 2, -1)

                               (0, 0, 0) // <-- queried cell
                               (2, 0, 0)
                               (1, 1, 0) // <-- neighbor
                               (1, 2, 0)

                               (0, 0, 1) // <-- neighbor
                               (2, 1, 1)
                               (1, 2, 1) ]

              let cell = (0, 0, 0)
              let count_expected = 3

              let count_actual = countNeighbors state cell

              Expect.equal count_actual count_expected ""
          }
          test "Can get first iteration" {
              let input = ".#.
..#
###"

              let i0 = parse input

              (*
              z=-1
              #..
              ..#
              .#.

              z=0
              #.#
              .##
              .#.

              z=1
              #..
              ..#
              .#.
              *)

              let i1_expected =
                  Set.ofList [ (0, 0, -1)
                               (2, 1, -1)
                               (1, 2, -1)

                               (0, 0, 0)
                               (2, 0, 0)
                               (1, 1, 0)
                               (1, 2, 0)

                               (0, 0, 1)
                               (2, 1, 1)
                               (1, 2, 1) ]

              let i1_actual = run i0 1

              Expect.equal i1_actual i1_expected ""
          } ]
