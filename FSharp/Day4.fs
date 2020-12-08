// https://adventofcode.com/2020/day/4
module Day4

open Expecto

type Record = seq<string * string>

let parseRecord (input: string) =
    input.Split([| '\n'; ' ' |])
    |> Seq.map
        (fun kvptok ->
            let kvp = kvptok.Split(":")
            (kvp.[0], kvp.[1]))

let parseAll (input: string): seq<Record> =
    // handling for inline test data with non-unix line endings
    let input = input.Replace("\r\n", "\n")

    let records = input.Split("\n\n")

    Seq.map parseRecord records

let isValid (record: Record): bool =
    let required =
        Set.ofList [ "byr"
                     // "cid"  <-- optional
                     "iyr"
                     "eyr"
                     "hgt"
                     "hcl"
                     "ecl"
                     "pid" ]

    let keys =
        record |> Seq.map (fun (k, _) -> k) |> Set.ofSeq

    let missing = Seq.except keys required
    Seq.isEmpty missing

let part1 (input: string): int =
    parseAll input |> Seq.filter isValid |> Seq.length

let yearValid (min: int) (max: int) (token: string): bool =
    let result = System.Int32.TryParse token

    match result with
    | (true, year) when year >= min && year <= max -> true
    | _ -> false

let validateField (key: string) (value: string): bool =
    let validator =
        match key with
        | "byr" -> yearValid 1920 2002
        | "iyr" -> yearValid 2010 2020
        | "eyr" -> yearValid 2020 2030
        | _ -> (fun _ -> false)

    validator value

let validateFields (record: Record): bool =
    let required =
        Set.ofList [ "byr"
                     // "cid"  <-- optional
                     "iyr"
                     "eyr"
                     "hgt"
                     "hcl"
                     "ecl"
                     "pid" ]


    false


let solve (input: string): (int * int) = part1 input, 0

let example = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let yearValidatorTests =
    [ ("1919", false)
      ("1920", true)
      ("2002", true)
      ("2003", false)
      ("abc", false) ]
    |> List.map
        (fun (input, expected) ->
            let description = $"year validity: {input} {expected}"
            test description { Expect.equal (yearValid 1920 2002 input) expected "" })

let tests =
    testList
        "Day 4"
        ([ test "Can parse input - split into records" {
            let recordCount_expected = 4

            let recordCount_actual = Seq.length (parseAll example)

            Expect.equal recordCount_actual recordCount_expected ""
           }
           test "Can parse input - split record into key-value-pairs (first)" {
               let head_expected =
                   [ ("ecl", "gry")
                     ("pid", "860033327")
                     ("eyr", "2020")
                     ("hcl", "#fffffd")
                     ("byr", "1937")
                     ("iyr", "2017")
                     ("cid", "147")
                     ("hgt", "183cm") ]

               let head_actual: Record = Seq.head (parseAll example)

               Expect.equal (List.ofSeq head_actual) head_expected ""
           }
           test "Part 1 - Can validate known-good record" {
               let record =
                   [ ("ecl", "gry")
                     ("pid", "860033327")
                     ("eyr", "2020")
                     ("hcl", "#fffffd")
                     ("byr", "1937")
                     ("iyr", "2017")
                     ("cid", "147")
                     ("hgt", "183cm") ]

               Expect.isTrue (isValid record) ""
           }
           test "Part 1 - Can not validate known-bad record" {
               // Missing hgt
               let record =
                   [ ("iyr", "2013")
                     ("ecl", "amb")
                     ("cid", "350")
                     ("eyr", "2023")
                     ("pid", "028048884")
                     ("hcl", "#cfa07d")
                     ("byr", "1929") ]

               Expect.isFalse (isValid record) ""
           }
           test "Part 1 - Validates all correctly" {
               let validCount_Expected = 2

               let validCount_Actual = part1 example

               Expect.equal validCount_Expected validCount_Expected ""
           } ]
         @ yearValidatorTests)
