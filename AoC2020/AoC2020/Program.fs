// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

module Program

let day1 argv =
    let args = Array.map int argv
    Day1.firstTask (Array.toList args)

let day2 argv =
    Day2.secondTask argv

let day3 argv =
    Day3.secondTask argv

let day4 (arg: string) =
    let args = arg.Split "\n\n" |> Array.toList
    Day4.secondTask args

let day5 argv =
    Day5.secondTask argv

let day6 (arg: string) =
    let args = arg.Split "\n\n"
    Day6.secondTask args

let day7 (arg: string) =
    let args = arg.Split '\n'
    Day7.secondTask args

let day8 (arg: string) =
    let args = arg.Split '\n'
    Day8.secondTask args

let day9 argv =
    Day9.secondTask argv

let day10 argv =
    Day10.secondTask argv

let day11 argv =
    Day11.secondTask argv

let day12 argv =
    Day12.secondTask argv

let day13 argv =
    Day13.firstTask argv

let day14 (arg: string) =
    let args = arg.Split '\n'
    Day14.secondTask args

[<EntryPoint>]
let main argv =
    let message = day14 Day14Input.input
    printfn "%d" message
    0 // return an integer exit code