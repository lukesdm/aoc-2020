// https://adventofcode.com/2020/day/5
module Day5

open Expecto

type Bounds = { Lower: int; Upper: int }

type SeatSearch = { Row: Bounds; Col: Bounds }

type Seat = { Row: int; Col: int }

let findSeat (input: string): Seat = { Row = 0; Col = 0 }

// BFFFBBFRRR: row 70, column 7, seat ID 567
let example1 = "BFFFBBFRRR"

let tests =
    testList
        "Day 5"
        [ test "Find row and col - example 1" {

            let seat_expected: Seat = { Row = 70; Col = 7 }

            let seat_actual = findSeat example1

            Expect.equal seat_actual seat_expected ""
          } ]
