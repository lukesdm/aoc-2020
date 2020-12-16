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

let nextSeatState seats row col =
    let neighbors = countNeighbors seats row col

    if seats.[row, col] = Floor then Floor
    else if neighbors = 0 then Occupied
    else if neighbors >= 4 then Empty
    else seats.[row, col]

let next (seats: Seat [,]): Seat [,] =
    seats
    |> Array2D.mapi (fun row col _ -> nextSeatState seats row col)

/// Runs until stable, given the initial state. Returns the number of iterations and final state
let rec run (i: int) (seats: Seat [,]): int * Seat [,] =
    let nxt = next seats
    if seats = nxt then (i, seats) else run (i + 1) nxt

/// A helper function for 2D arrays, which aren't compatible with Seq functions
let countWhere (arr: 'TItem [,]) (predicate: 'TItem -> bool): int =
    let mutable count = 0
    Array2D.iteri (fun _ _ item -> count <- count + if predicate item then 1 else 0) arr
    count

// Attempt at recursive version of above - there's a problem (see TODO), which in fixing would probably end up with this being way more messy than the above
//let rec countWhereInner acc row col arr predicate =
//    if not (isInside arr row col) then
//        (acc, row, col)
//    else
//        let o = if (predicate arr.[row, col]) then 1 else 0
//        // TODO: Fix (don't add 1 to both row AND cols here)
//        countWhereInner (acc + o) (row + 1) (col + 1) arr predicate

//let countWhere arr predicate =
//    let (result, _, _) = countWhereInner 0 0 0 arr predicate
//    result

let part1 input =
    let (_, seats) = parse input |> run 0
    countWhere seats (fun seat -> seat = Occupied)

let solve = part1

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
          test "Becomes stable at 6th iteration" {
              let i0 = parse example0

              let (i, _) = run 0 i0

              Expect.equal i 5 ""
          }
          test "Part 1 - Can solve for example" {
              let result = part1 example0

              Expect.equal result 37 ""
          } ]
