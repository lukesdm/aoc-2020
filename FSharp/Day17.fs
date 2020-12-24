/// https://adventofcode.com/2020/day/17
///
/// TL;DR: Game of Life 3D
module Day17

open Expecto
open System

/// Cell position: X, Y, Z
type Cell = int * int * int

/// Collection of active cells
type Cells = Set<Cell>

let toXyz rowCount colCount row col =
    (col - colCount / 2, row - rowCount / 2, 0)

/// Parses the input string into a collection of active cells
let parse (input: string): Cells =
    let lines =
        input.Replace("\r\n", "\n").Split("\n")
        |> Array.indexed

    seq {
        for (row, line) in lines do
            for col in 0 .. lines.Length - 1 do
                if line.[col] = '#'
                then yield toXyz lines.Length line.Length row col
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

type Bounds = Cell * Cell
/// Gets the bounding box for the given active cells
let getBounds (activeCells: Cells): Bounds =
    let init =
        ((Int32.MaxValue, Int32.MaxValue, Int32.MaxValue), (Int32.MinValue, Int32.MinValue, Int32.MinValue))

    activeCells
    |> Seq.fold
        (fun currBounds cell ->
            let ((minX, minY, minZ), (maxX, maxY, maxZ)) = currBounds
            let (cX, cY, cZ) = cell
            ((min cX minX, min cY minY, min cZ minZ), (max cX maxX, max cY maxY, max cZ maxZ)))
        init

/// Given a set of active cells, returns the next generation of active cells
let next (activeCells: Cells): Cells =
    (*
    During a cycle, all cubes simultaneously change their state according to the following rules:

    If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
    If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
    *)

    let isActive =
        fun cell -> Seq.contains cell activeCells

    let ((xMin, yMin, zMin), (xMax, yMax, zMax)) = getBounds activeCells

    // Cell space: bounding box with 1-cell padding
    let cells =
        seq {
            for x in xMin - 1 .. xMax + 1 do
                for y in yMin - 1 .. yMax + 1 do
                    for z in zMin - 1 .. zMax + 1 do
                        yield (x, y, z)
        }

    seq {
        for cell in cells do
            let nc = countNeighbors activeCells cell

            match nc with
            | 2 when (isActive cell) -> yield cell
            | 3 -> yield cell
            | _ -> ()
    }
    |> Set.ofSeq

let gen i0 =
    i0
    |> Seq.unfold
        (fun state ->
            let nxt = next state
            Some(nxt, nxt))

// Given initial state i0, runs the generator for n iterations.
let run i0 n =
    if n = 0 then i0 else gen i0 |> Seq.item (n - 1)

let part1 input =
    let i0 = parse input
    let i6 = run i0 6
    i6.Count

// ***PART 2***
// Mostly a copy and paste of part1, then adding 4th dimension W

/// Cell position: X, Y, Z, W
type Cell4D = int * int * int * int

/// Collection of active cells
type Cell4Ds = Set<Cell4D>

let toXyzw rowCount colCount row col =
    (col - colCount / 2, row - rowCount / 2, 0, 0)

/// Parses the input string into a collection of active cells
let parse4D (input: string): Cell4Ds =
    let lines =
        input.Replace("\r\n", "\n").Split('\n')
        |> Array.indexed

    seq {
        for (row, line) in lines do
            for col in 0 .. lines.Length - 1 do
                if line.[col] = '#'
                then yield toXyzw lines.Length line.Length row col
    }
    |> Set.ofSeq

let countNeighbors4D cells (x, y, z, w) =
    seq {
        for dx in -1 .. 1 do
            for dy in -1 .. 1 do
                for dz in -1 .. 1 do
                    for dw in -1 .. 1 do
                        if (dx, dy, dz, dw) <> (0, 0, 0, 0) then yield (dx, dy, dz, dw)
    }
    |> Seq.filter (fun (dx, dy, dz, dw) -> Set.contains (x + dx, y + dy, z + dz, w + dw) cells)
    |> Seq.length

type Bounds4D = Cell4D * Cell4D
/// Gets the bounding box for the given active cells
let getBounds4D (activeCells: Cell4Ds): Bounds4D =
    let init =
        ((Int32.MaxValue, Int32.MaxValue, Int32.MaxValue, Int32.MaxValue), (Int32.MinValue, Int32.MinValue, Int32.MinValue, Int32.MinValue))

    activeCells
    |> Seq.fold
        (fun currBounds cell ->
            let ((minX, minY, minZ, minW), (maxX, maxY, maxZ, maxW)) = currBounds
            let (cX, cY, cZ, cW) = cell
            ((min cX minX, min cY minY, min cZ minZ, min cW minW), (max cX maxX, max cY maxY, max cZ maxZ, max cW maxW)))
        init

/// Given a set of active cells, returns the next generation of active cells
let next4D (activeCells: Cell4Ds): Cell4Ds =
    (*
    During a cycle, all cubes simultaneously change their state according to the following rules:

    If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
    If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
    *)

    let isActive =
        fun cell -> Seq.contains cell activeCells

    let ((xMin, yMin, zMin, wMin), (xMax, yMax, zMax, wMax)) = getBounds4D activeCells

    // Cell space: bounding box with 1-cell padding
    let cells =
        seq {
            for x in xMin - 1 .. xMax + 1 do
                for y in yMin - 1 .. yMax + 1 do
                    for z in zMin - 1 .. zMax + 1 do
                        for w in wMin - 1 .. wMax + 1 do
                            yield (x, y, z, w)
        }

    seq {
        for cell in cells do
            let nc = countNeighbors4D activeCells cell

            match nc with
            | 2 when (isActive cell) -> yield cell
            | 3 -> yield cell
            | _ -> ()
    }
    |> Set.ofSeq

let gen4D i0 =
    i0
    |> Seq.unfold
        (fun state ->
            let nxt = next4D state
            Some(nxt, nxt))

// Given initial state i0, runs the generator for n iterations.
let run4D i0 n =
    if n = 0 then i0 else gen4D i0 |> Seq.item (n - 1)

let part2 input =
    let i0 = parse4D input
    let i6 = run4D i0 6
    i6.Count
// ***...***

let solve input = (part1 input, part2 input)

let tests =
    testList
        "Day17"
        [ test "Part 1 - Can Parse" {
            // X = Left to Right
            // Y = Top to Bottom
            // Z = Far to Near
            let input = ".#.
..#
###"

            let activeCells_expected =
                Set.ofList [ (0, -1, 0)
                             (1, 0, 0)
                             (-1, 1, 0)
                             (0, 1, 0)
                             (1, 1, 0) ]

            let activeCells_actual = parse input

            Expect.equal activeCells_actual activeCells_expected ""
          }
          test "Part 1 - Can count active neighbors" {
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
          test "Part 1- Can get bounding box" {
              let state =
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

              let bounds_expected = ((0, 0, -1), (2, 2, 1))

              let bounds_actual = getBounds state

              Expect.equal bounds_actual bounds_expected ""
          }
          test "Part 1 - Simple" {
              //z= 0      -1    0    1
              //  .#.     ...  ...  ...
              //  ..# --> .##  .##  .##
              //  .#.     ...  ...  ...

              let i0 =
                  Set.ofList [ (0, -1, 0)
                               (1, 0, 0)
                               (0, 1, 0) ]

              let i1_expected =
                  Set.ofList [ (0, 0, -1)
                               (1, 0, -1)
                               (0, 0, 0)
                               (1, 0, 0)
                               (0, 0, 1)
                               (1, 0, 1) ]

              let i1_actual = next i0

              Expect.equal i1_actual i1_expected ""
          }
          test "Part 1 - Can get first iteration" {
              let input = ".#.
..#
###"
              let i0 = parse input

              // Note: These come from the AoC page, where it seems like the grid showing these is centered at 0, 1, 0 not 0, 0, 0 as it first appears.
              // z=-1    0      1
              // #..    #.#    #..
              // ..#    .##    ..#
              // .#.    .#.    .#.
              let i1_expected =
                  Set.ofList [ (-1, 0, -1)
                               (1, 1, -1)
                               (0, 2, -1)

                               (-1, 0, 0)
                               (1, 0, 0)
                               (0, 1, 0)
                               (1, 1, 0)
                               (0, 2, 0)

                               (-1, 0, 1)
                               (1, 1, 1)
                               (0, 2, 1) ]

              let i1_actual = run i0 1

              Expect.equal i1_actual i1_expected ""
          }
          test "Part 1 - Correct active cell count after 6" {
              let input = ".#.
..#
###"
              let i0 = parse input
              let count_expected = 112

              let i6 = run i0 6
              let count_actual = i6.Count

              Expect.equal count_actual count_expected ""
          }
          test "Part 2 - Correct active cell count after 6" {
              let input = ".#.
..#
###"
              let i0 = parse4D input
              let count_expected = 848

              let i6 = run4D i0 6
              let count_actual = i6.Count

              Expect.equal count_actual count_expected ""
          } ]
