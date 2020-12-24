﻿/// https://adventofcode.com/2020/day/24
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

let add (q1, r1) (q2, r2) = (q1 + q2, r1 + r2)

/// See QR (axial) coordinate system from https://www.redblobgames.com/grids/hexagons/
let qrCoords directions =
    directions
    |> Seq.fold
        (fun acc dir ->
            add
                acc
                (match dir with
                 | E -> (1, 0)
                 | SE -> (0, 1)
                 | SW -> (-1, 1)
                 | W -> (-1, 0)
                 | NW -> (0, -1)
                 | NE -> (1, -1)))
        (0, 0)

let part1 (input: string): int =
    // QR coordinates of tiles to be flipped
    let flipsQR =
        input.Replace("\r\n", "\n").Split('\n')
        |> Seq.map parseLine
        |> Seq.map qrCoords

    // All tiles start white. An odd number of flips of tile at a specific coord means that the tile will end up black
    flipsQR
    |> Seq.countBy (fun coord -> coord)
    |> Seq.filter (fun (_, count) -> count % 2 = 1)
    |> Seq.length

let solve input = part1 input

let example = "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"

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
          }
          test "Can calculate QR tile coordinates (1)" {
              // "a line like esew flips a tile immediately adjacent to the reference tile"
              let directions = [ E; SE; W ]

              let coords_expected = (0, 1)

              let coords_actual = qrCoords directions

              Expect.equal coords_actual coords_expected ""
          }
          test "Can calculate QR tile coordinates (2)" {
              // "a line like nwwswee flips the reference tile itself"
              let directions = [ NW; W; SW; E; E ]

              let coords_expected = (0, 0)

              let coords_actual = qrCoords directions

              Expect.equal coords_actual coords_expected ""
          }
          test "Can count number of tiles flipped to black" {
              // "a line like nwwswee flips the reference tile itself"
              let input = example

              let count_expected = 10

              let count_actual = part1 input

              Expect.equal count_actual count_expected ""
          } ]
