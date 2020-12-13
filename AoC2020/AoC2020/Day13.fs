module Day13
    let rec convertToIds ss =
        match ss with
        | []    -> []
        | n::ns ->
            match n with
            | "x" -> convertToIds ns
            | _   -> int n::convertToIds ns

    let getBusIds (s: string) =
        s.Split ',' |> Array.toList |> convertToIds

    let getWaitingTime earlistDeparture line =
        (earlistDeparture / line + 1) * line - earlistDeparture

    let keepSmallest (line1, waiting1) (line2, waiting2) =
        if waiting1 < waiting2 then
            (line1, waiting1)
        else
            (line2, waiting2)

    let firstTask (argv: string[]) =
        let departure = int argv.[0]
        getBusIds argv.[1] |> List.map (fun l -> (l, getWaitingTime departure l)) |> List.reduce keepSmallest |> fun (a, b) -> a * b