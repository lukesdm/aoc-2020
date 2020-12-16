﻿module Day11

open Expecto

let example = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

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


// TODO: Implement
let countNeighbours (seats: Seat [,]) row col = 0


//let nextSeatState seats (row, col)  =
let nextSeatState seats row col =
    let neighbors = countNeighbours seats row col

    if seats.[row, col] = Floor then Floor
    else if neighbors = 0 then Occupied
    else if neighbors = 4 then Empty
    else seats.[row, col]

let next (seats: Seat [,]): Seat [,] =
    seats
    |> Array2D.mapi (fun row col _ -> nextSeatState seats row col)

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

              printfn "%s" (format actual)

              Expect.equal actual expected ""
          }
          test "Can get second iteration" {
              let i1 = parse example1
              let expected = parse example2

              let actual = next i1

              printfn "%s" (format actual)

              Expect.equal actual expected ""
          } ]
