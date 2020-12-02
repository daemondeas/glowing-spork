// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Program

[<EntryPoint>]
let main argv =
    let args = Array.map int argv
    let message = Day1.secondTask (Array.toList args)
    printfn "%d" message
    0 // return an integer exit code