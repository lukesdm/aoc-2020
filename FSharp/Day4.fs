// https://adventofcode.com/2020/day/4
module Day4

open Expecto
open System.Text.RegularExpressions

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

let heightValid (token: string): bool =
    let ustart = token.Length - 2
    let unit = token.[ustart..]

    match System.Int32.TryParse token.[..ustart - 1] with
    | (true, height) when unit = "cm" -> height >= 150 && height <= 193
    | (true, height) when unit = "in" -> height >= 59 && height <= 76
    | _ -> false

let hairColorRegex = new Regex("^#[0-9a-f]{6}$")
let hairColorValid (token: string): bool = hairColorRegex.IsMatch token

let eyeColorValid (token: string): bool =
    let validColors =
        Set.ofList [ "amb"
                     "blu"
                     "brn"
                     "gry"
                     "grn"
                     "hzl"
                     "oth" ]

    validColors.Contains token

let pidRegex = new Regex("^\d{9}$")
let pidValid (token: string): bool = pidRegex.IsMatch token

let validateField (key: string) (value: string): bool =
    let validator =
        match key with
        | "byr" -> yearValid 1920 2002
        | "iyr" -> yearValid 2010 2020
        | "eyr" -> yearValid 2020 2030
        | "hgt" -> heightValid
        | "hcl" -> hairColorValid
        | "ecl" -> eyeColorValid
        | "pid" -> pidValid
        | _ -> (fun _ -> false)

    validator value

let validateRecord (record: Record): bool =
    let requiredFields =
        Set.ofList [ "byr"
                     // "cid"  <-- optional
                     "iyr"
                     "eyr"
                     "hgt"
                     "hcl"
                     "ecl"
                     "pid" ]

    let fields = Map.ofSeq record

    let valid =
        requiredFields
        |> Seq.filter
            (fun key ->
                fields.ContainsKey(key)
                && validateField key (fields.Item(key)))

    Seq.length valid = Seq.length requiredFields

let part2 (input: string): int =
    parseAll input
    |> Seq.filter validateRecord
    |> Seq.length

let solve (input: string): (int * int) = part1 input, part2 input

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

//(Height) - a number followed by either cm or in:
//If cm, the number must be at least 150 and at most 193.
//If in, the number must be at least 59 and at most 76.
let heightValidatorTests =
    [ ("149cm", false)
      ("150cm", true)
      ("193cm", true)
      ("194cm", false)
      ("58in", false)
      ("59in", true)
      ("76in", true)
      ("77in", false)
      ("150", false)
      ("cm", false)
      ("abc", false)
      ("a", false) ]
    |> List.map
        (fun (input, expected) ->
            let description = $"height validity: {input} {expected}"
            test description { Expect.equal (heightValid input) expected "" })

// (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
let hairColorValidatorTests =
    [ ("#a2cd56", true)
      ("#01234g", false)
      ("#a2cd567", false)
      ("#a", false)
      ("a", false) ]
    |> List.map
        (fun (input, expected) ->
            let description =
                $"hair color validity: {input} {expected}"

            test description { Expect.equal (hairColorValid input) expected "" })

// (Eye Color) - exactly one of: amb blu brn gry grn hzl oth
let eyeColorValidatorTests =
    [ ("amb", true)
      ("blu", true)
      ("brn", true)
      ("gry", true)
      ("hzl", true)
      ("oth", true)
      ("xxx", false)
      ("a", false) ]
    |> List.map
        (fun (input, expected) ->
            let description =
                $"eye color validity: {input} {expected}"

            test description { Expect.equal (eyeColorValid input) expected "" })

// (Passport ID) - a nine-digit number, including leading zeroes
let pidValidatorTests =
    [ ("123456789", true)
      ("003456789", true)
      ("12345678", false)
      ("abcdefghi", false) ]
    |> List.map
        (fun (input, expected) ->
            let description =
                $"passport id validity: {input} {expected}"

            test description { Expect.equal (pidValid input) expected "" })

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
         @ yearValidatorTests
           @ heightValidatorTests
             @ hairColorValidatorTests
               @ eyeColorValidatorTests @ pidValidatorTests)
