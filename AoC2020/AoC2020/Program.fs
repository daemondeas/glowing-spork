// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Program

let day1 argv =
    let args = Array.map int argv
    Day1.firstTask (Array.toList args)

let day2 argv =
    Day2.secondTask argv

let day3 argv =
    Day3.secondTask argv

[<EntryPoint>]
let main argv =
    let message = day3 argv
    printfn "%d" message
    0 // return an integer exit code