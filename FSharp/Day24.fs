/// https://adventofcode.com/2020/day/24
module Day24

open Expecto
open System
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

// ***PART 2***

type TileQR = int * int
type Tiles = Set<TileQR>

/// Calculates the initial state, as the set of black tiles
let initialState (input: string): Tiles =
    // Similar to part 1, get black tiles by determining those with an odd number of flips
    let flipsQR =
        input.Replace("\r\n", "\n").Split('\n')
        |> Seq.map parseLine
        |> Seq.map qrCoords

    flipsQR
    |> Seq.countBy (fun coord -> coord)
    |> Seq.filter (fun (_, count) -> count % 2 = 1)
    |> Seq.map (fun (coord, _) -> coord)
    |> Set.ofSeq

let countNeighbors blackTiles (q, r) =
    seq {
        (-1, 0)
        (0, -1)
        (1, -1)
        (1, 0)
        (0, 1)
        (-1, 1)
    }
    |> Seq.filter (fun (dq, dr) -> Set.contains (q + dq, r + dr) blackTiles)
    |> Seq.length

let getBounds (blackTiles: Tiles): TileQR * TileQR =
    let init =
        ((Int32.MaxValue, Int32.MaxValue), (Int32.MinValue, Int32.MinValue))

    blackTiles
    |> Seq.fold
        (fun currBounds tile ->
            let ((minQ, minR), (maxQ, maxR)) = currBounds
            let (cQ, cR) = tile
            ((min cQ minQ, min cR minR), (max cQ maxQ, max cR maxR)))
        init

/// Calculate the next generation of the layout, given the current set of black tiles.
let next (blackTiles: Tiles): Tiles =
    let isBlack = fun cell -> Seq.contains cell blackTiles

    let ((qMin, rMin), (qMax, rMax)) = getBounds blackTiles

    // Cell space: bounding box with 1-tile padding
    let tiles =
        seq {
            for q in qMin - 1 .. qMax + 1 do
                for r in rMin - 1 .. rMax + 1 do
                    yield (q, r)
        }

    seq {
        for tile in tiles do
            let nc = countNeighbors blackTiles tile

            match nc with
            | 1 when isBlack tile -> yield tile
            | 2 -> yield tile
            | _ -> ()
    }
    |> Set.ofSeq

/// Sequence of generations, indexable by day. Infinite.
let gen i0 =
    let gens =
        i0
        |> Seq.unfold
            (fun state ->
                let nxt = next state
                Some(nxt, nxt))

    // Yield sequence, with intial state added back in at start.
    seq {
        i0
        yield! gens
    }

let part2 (input: string): int =
    initialState input
    |> gen
    |> Seq.item 100
    |> Seq.length

// ***...***

let solve input = (part1 input, part2 input)

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
          }
          test "Part 2 - Count correct number of tiles given the example pattern, on day 1" {
              let input = example
              let i0 = initialState input

              let count_expected = 15

              let count_actual = Seq.length (next i0)

              Expect.equal count_actual count_expected ""
          }
          test "Part 2 - Simple pattern, day 1" {
              let i0: Tiles = [ (-1, 0); (0, -1) ] |> Set.ofList

              let i1_expected =
                  [ (-1, 0); (0, -1); (0, 0); (-1, -1) ]
                  |> Set.ofList

              let i1_actual = next i0

              Expect.equal i1_actual i1_expected ""
          }
          test "Part 2 - Simple pattern, day 2" {
              let i1: Tiles =
                  [ (-1, 0); (0, -1); (0, 0); (-1, -1) ]
                  |> Set.ofList

              let i2_expected =
                  [ (0, 0)
                    (-1, -1)
                    (-1, 1)
                    (-2, 0)
                    (0, -2)
                    (1, -1) ]
                  |> Set.ofList

              let i2_actual = next i1

              Expect.equal (List.ofSeq i2_actual) (List.ofSeq i2_expected) ""
          }
          test "Part 2 - Have correct number of tiles after day 2" {
              let input = example
              let i0 = initialState input

              let count_expected = 12

              let results = i0 |> gen |> Seq.item (2)
              let count_actual = Seq.length results

              Expect.equal count_actual count_expected ""
          }
          test "Part 2 - Have correct number of tiles up to day 100" {
              let input = example
              let i0 = initialState input

              let counts_expected =
                  [ (1, 15)
                    (2, 12)
                    (3, 25)
                    (4, 14)
                    (5, 23)
                    (6, 28)
                    (20, 132)
                    (100, 2208) ]

              let results = i0 |> gen |> Seq.cache

              let counts_actual =
                  counts_expected
                  |> Seq.map (fun (i, _) -> (i, Seq.item i results |> Seq.length))
                  |> List.ofSeq

              Expect.equal counts_actual counts_expected ""
          } ]
