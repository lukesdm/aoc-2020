open System

 // TODO: Implement
let d1p1 = 12
let d1p2 = 24

let solve_day1 = (d1p1, d1p2)

let parseArgs (args: string[]) : Option<string> =
    if args.Length = 1 then Some(args.[0]) else None

[<EntryPoint>]
let main argv =
    let day = 
        match (parseArgs argv) with
        | Some(day) -> day
        | None ->
            printfn "Usage: expected a single argument of the form {DAY}, e.g. '1'"
            exit 1
    
    let result = 
        match day with
        | "1" -> solve_day1
        | _ ->
            printfn $"Not implemented for day {day}"
            exit 1
    printfn $"Result for day {day}: {result}"
    0