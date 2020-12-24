/// https://adventofcode.com/2020/day/24
module Day24

open Expecto
open System.Text.RegularExpressions

/// "directions are given in your list, respectively, as e, se, sw, w, nw, and ne"
type Direction =
    | E
    | SE
    | SW
    | W
    | NW
    | NE

let parseLine (line: string): seq<Direction> =
    // Use regex's eagerness to split the string
    Regex.Matches(line, "(se|e)|(sw|w)|(ne|e)|(nw|w)")
    |> Seq.map
        (fun m ->
            match m.Value with
            | "e" -> E
            | "se" -> SE
            | "sw" -> SW
            | "w" -> W
            | "nw" -> NW
            | "ne" -> NE
            | _ -> failwith "Unexpected character")

let tests =
    testList
        "Day24"
        [ test "Can parse line into directions" {
            let line = "sesenwnenenewseeswwswswwnenewsewsw"

            let directions_expected =
                [ SE
                  SE
                  NW
                  NE
                  NE
                  NE
                  W
                  SE
                  E
                  SW
                  W
                  SW
                  SW
                  W
                  NE
                  NE
                  W
                  SE
                  W
                  SW ]

            let directions_actual = parseLine line

            Expect.equal (List.ofSeq directions_actual) directions_expected ""
          } ]
