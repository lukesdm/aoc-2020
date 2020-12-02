open Expecto
open System
open Day1

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

    let result =
        match day with
        | "1" ->
            let _ = runTestsWithCLIArgs [] [||] tests
            Day1.solve
        | _ ->
            printfn $"Not implemented for day {day}"
            exit 1

    printfn $"Result for day {day}: {result}"
    0
