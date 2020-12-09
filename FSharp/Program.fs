open Expecto
open System
open System.IO
open Day1
open Day2

let parseArgs (args: string []): Option<string> =
    if args.Length = 1 then Some(args.[0]) else None

[<EntryPoint>]
let main argv =
    let day =
        match (parseArgs argv) with
        | Some (day) -> day
        | None ->
            printfn "Usage: expected a single argument of the form {DAY}, e.g. '1'"
            exit 1

    let result: Object =
        match day with
        | "1" ->
            let _ = runTestsWithCLIArgs [] [||] Day1.tests

            let input =
                File.ReadAllLines "day1.txt"
                |> Array.map (fun s -> int s)

            Day1.solve input :> Object
        | "2" ->
            let _ = runTestsWithCLIArgs [] [||] Day2.tests

            let input = File.ReadAllLines "day2.txt"

            Day2.solve input :> Object
        | "3" ->
            let _ = runTestsWithCLIArgs [] [||] Day3.tests

            let input = File.ReadAllLines "day3.txt"

            Day3.solve input :> Object
        | "4" ->
            let _ = runTestsWithCLIArgs [] [||] Day4.tests

            let input = File.ReadAllText "day4.txt"

            Day4.solve (input.TrimEnd()) :> Object
        | "5" ->
            let _ = runTestsWithCLIArgs [] [||] Day5.tests
            let input = File.ReadAllLines "day5.txt"
            Day5.solve input :> Object
        | _ ->
            printfn $"Not implemented for day {day}"
            exit 1

    printfn $"Result for day {day}: {result}"
    0
