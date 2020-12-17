module Day11

open Expecto

type Seat =
    | Empty
    | Occupied
    | Floor

let parseRow (line: string): seq<Seat> =
    line
    |> Seq.map
        (fun c ->
            match c with
            | 'L' -> Empty
            | '.' -> Floor
            | '#' -> Occupied
            | _ -> failwith "Unexpected character")

/// Parses the input text into the initial state
let parse (input: string): Seat [,] =
    input.Replace("\r\n", "\n").Split("\n")
    |> Seq.map parseRow
    |> array2D

let toChar (seat: Seat): char =
    match seat with
    | Empty -> 'L'
    | Floor -> '.'
    | Occupied -> '#'

let format (seats: Seat [,]): string =
    let sb = System.Text.StringBuilder()

    for row in 0 .. Array2D.length1 seats - 1 do
        for col in 0 .. Array2D.length2 seats - 1 do
            let c = toChar seats.[row, col]
            sb.Append(c) |> ignore

        sb.AppendLine() |> ignore

    sb.ToString()

let isInside seats row col =
    row >= 0
    && col >= 0
    && row < Array2D.length1 seats
    && col < Array2D.length2 seats

let countNeighbors (seats: Seat [,]) row col =
    seq {
        for dRow in -1 .. 1 do
            for dCol in -1 .. 1 do
                if isInside seats (row + dRow) (col + dCol)
                   && (dRow, dCol) <> (0, 0) then
                    yield (dRow, dCol)
    }
    |> Seq.filter (fun (dRow, dCol) -> seats.[row + dRow, col + dCol] = Occupied)
    |> Seq.length

/// Calculates the next state for the given seat
let nextSeatState seats row col =
    let neighbors = countNeighbors seats row col

    if seats.[row, col] = Floor then Floor
    else if neighbors = 0 then Occupied
    else if neighbors >= 4 then Empty
    else seats.[row, col]

/// Calculates the next iteration of seat arrangement
let next (seats: Seat [,]): Seat [,] =
    seats
    |> Array2D.mapi (fun row col _ -> nextSeatState seats row col)

/// Runs until stable, given the initial state. Returns the number of iterations and final state
let rec run (i: int) (seats: Seat [,]): int * Seat [,] =
    let nxt = next seats
    if seats = nxt then (i, seats) else run (i + 1) nxt

/// Generator of seat arrangements. Infinite.
let seatsSeq (i0: Seat [,]) =
    i0
    |> Seq.unfold
        (fun state ->
            let nxt = next state
            Some(nxt, nxt))

/// Runs seat arrangment generator until stable, given the initial state. Returns the number of iterations and final state
let runGen (i0: Seat [,]): int * Seat [,] =
    i0
    |> seatsSeq
    |> Seq.indexed
    |> Seq.pairwise
    |> Seq.takeWhile (fun (prev, curr) -> snd prev <> snd curr)
    |> Seq.last
    |> snd

/// A helper function for 2D arrays, which aren't compatible with Seq functions
let countWhere (arr: 'TItem [,]) (predicate: 'TItem -> bool): int =
    let mutable count = 0
    Array2D.iteri (fun _ _ item -> count <- count + if predicate item then 1 else 0) arr
    count

let part1 input =
    let (_, seats) = parse input |> run 0
    countWhere seats (fun seat -> seat = Occupied)

// ***PART 2***

/// Check whether there's an occupied seat, by marching along a vector
let rec checkHit (posRow, posCol) (seats: Seat [,]) (dRow, dCol): bool =
    let (newRow, newCol) = (posRow + dRow, posCol + dCol)

    if (not (isInside seats newRow newCol))
       || seats.[newRow, newCol] = Empty then
        false
    else if seats.[newRow, newCol] = Occupied then
        true
    else
        checkHit (newRow, newCol) seats (dRow, dCol)

/// Part 2 - Count neighbors by line of sight in 8 directions
let countNeighbors2 (seats: Seat [,]) (row: int) (col: int): int =
    let vectors =
        seq {
            for dRow in -1 .. 1 do
                for dCol in -1 .. 1 do
                    if (dRow, dCol) <> (0, 0) then yield (dRow, dCol)
        }

    vectors
    |> Seq.filter (checkHit (row, col) seats)
    |> Seq.length

/// Part 2 - Calculates the next state for the given seat
let nextSeatState2 seats row col =
    let neighbors = countNeighbors2 seats row col

    if seats.[row, col] = Floor then Floor
    else if neighbors = 0 then Occupied
    else if neighbors >= 5 then Empty
    else seats.[row, col]

/// Part 2 - Calculates the next iteration of seat arrangement
let next2 (seats: Seat [,]): Seat [,] =
    seats
    |> Array2D.mapi (fun row col _ -> nextSeatState2 seats row col)

/// Part 2 - Generator of seat arrangements. Infinite.
let seatsSeq2 (i0: Seat [,]) =
    i0
    |> Seq.unfold
        (fun state ->
            let nxt = next2 state
            Some(nxt, nxt))

let inspect input =
    printfn "%s" (format input)
    input

/// Part 2 - Runs seat arrangment generator until stable, given the initial state. Returns the number of iterations and final state
let runGen2 (i0: Seat [,]): int * Seat [,] =
    i0
    |> seatsSeq2
    //|> Seq.map inspect
    |> Seq.indexed
    |> Seq.pairwise
    |> Seq.takeWhile (fun (prev, curr) -> snd prev <> snd curr)
    |> Seq.last
    |> snd

let part2 input =
    let (_, seats) = parse input |> runGen2
    countWhere seats (fun seat -> seat = Occupied)

// ***...***

let solve input = (part1 input), (part2 input)


let example0 = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

let example1 = "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"

let example2 = "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##"

let example3 = "#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##"


let tests =
    testList
        "Day11"
        [ test "Can Parse" {
            let input = "L.L
.LL"

            let expected =
                array2D [ [ Empty; Floor; Empty ]
                          [ Floor; Empty; Empty ] ]

            let actual = parse input

            Expect.equal actual expected ""
          }
          test "Can get first iteration" {
              let init = parse example0
              let expected = parse example1

              let actual = next init

              Expect.equal (format actual) (format expected) ""
          }
          test "Can get second iteration" {
              let i1 = parse example1
              let expected = parse example2

              let actual = next i1

              Expect.equal (format actual) (format expected) ""
          }
          test "Can get third iteration" {
              let i2 = parse example2
              let expected = parse example3

              let actual = next i2

              Expect.equal (format actual) (format expected) ""
          }
          test "Becomes stable at 6th iteration - recursive function" {
              let i0 = parse example0

              let (i, _) = run 0 i0

              Expect.equal i 5 ""
          }
          test "Becomes stable at 6th iteration - generator" {
              let i0 = parse example0

              let (i, _) = runGen i0

              // i0 is not part of sequence, so end is 1 lower.
              Expect.equal i 4 ""
          }
          test "Part 1 - Can solve for example" {
              let result = part1 example0

              Expect.equal result 37 ""
          }
          test "Part 2 - Can count neighbors (A)" {
              let input = ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#....."
              let seats = parse input
              let expected = 8

              let actual = countNeighbors2 seats 4 3

              Expect.equal actual expected ""
          }
          test "Part 2 - Can count neghbors (B)" {
              let input = ".............
.L.L.#.#.#.#.
............."
              let seats = parse input
              let expected = 0

              let actual = countNeighbors2 seats 1 1

              Expect.equal actual expected ""
          }
          test "Part 2 - Can solve for example" {
              let expected = 26

              let actual = part2 example0

              Expect.equal actual expected ""
          } ]
