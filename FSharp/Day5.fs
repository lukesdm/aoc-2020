// https://adventofcode.com/2020/day/5
module Day5

open Expecto

type Bounds = { Min: int; Max: int }

type SeatSearch = { Row: Bounds; Col: Bounds }

type Seat = { Row: int; Col: int; Id: int }

let midPoint (bounds: Bounds): int =
    bounds.Min + (1 + bounds.Max - bounds.Min) / 2

let newLower (bounds: Bounds): Bounds =
    { bounds with
          Max = midPoint bounds - 1 }

let newUpper (bounds: Bounds): Bounds = { bounds with Min = midPoint bounds }

let searchSeat (acc: SeatSearch) (c: char): SeatSearch =
    match c with
    | 'F' -> { acc with Row = newLower acc.Row }
    | 'B' -> { acc with Row = newUpper acc.Row }
    | 'L' -> { acc with Col = newLower acc.Col }
    | 'R' -> { acc with Col = newUpper acc.Col }
    | _ -> failwith "unexpected character"

let seat (ss: SeatSearch): Seat =
    assert ((ss.Row.Min = ss.Row.Max)
            && (ss.Col.Min = ss.Col.Max))

    // "Every seat also has a unique seat ID: multiply the row by 8, then add the column."
    let row = ss.Row.Min
    let col = ss.Col.Min
    let id = row * 8 + col
    { Row = row; Col = col; Id = id }

// Follow the given binary search path to find seat
let findSeat (input: string): Seat =
    let init: SeatSearch =
        { Row = { Min = 0; Max = 127 }
          Col = { Min = 0; Max = 7 } }

    input |> Seq.fold searchSeat init |> seat

let part1 (input: string []): int =
    input
    |> Seq.map (fun line -> (findSeat line).Id)
    |> Seq.max

// Find ID missing from list
let part2 (input: string []): int =
    let seatsTaken =
        input
        |> Seq.map (fun line -> (findSeat line).Id)
        |> Set.ofSeq

    { Set.minElement seatsTaken .. Set.maxElement seatsTaken }
    |> Seq.filter (fun id -> not (Set.contains id seatsTaken))
    |> Seq.head

let solve (input: string []): (int * int) = part1 input, part2 input

let tests =
    testList
        "Day 5"
        [ test "Find row and col - example 1" {
            // BFFFBBFRRR: row 70, column 7, seat ID 567
            let input = "BFFFBBFRRR"

            let seat_expected: Seat = { Row = 70; Col = 7; Id = 567 }

            let seat_actual = findSeat input

            Expect.equal seat_actual seat_expected ""
          } ]
