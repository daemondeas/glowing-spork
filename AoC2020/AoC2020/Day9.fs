module Day9
    let rec hasMatchInner ns a n =
        match ns with
        | []    -> false
        | b::bs -> a + b = n || hasMatchInner bs a n

    let rec hasMatch (ns: int64 list) n =
        match ns.Length with
        | 0 -> false
        | 1 -> false
        | _ -> hasMatchInner ns.[1..] ns.[0] n || hasMatch ns.[1..] n

    let rec findMisMatch preAmbleSize (ns: int64 list) =
        match (hasMatch ns.[..preAmbleSize - 1] ns.[preAmbleSize]) with
        | false -> ns.[preAmbleSize]
        | true  -> findMisMatch preAmbleSize ns.[1..]

    let rec findSumSet (ns: int64 list) index n =
        let sum = ns.[..index] |> List.reduce (+)
        if sum = n then
            ns.[..index]
        elif sum > n then
            findSumSet ns.[1..] 1 n
        else
            findSumSet ns (index + 1) n

    let getWeakness (ns: int64 list) =
        (List.min ns) + (List.max ns)

    let firstTask (argv: string[]) =
        Array.map int64 argv |> Array.toList |> findMisMatch 25

    let secondTask argv =
        firstTask argv |> (findSumSet (Array.map int64 argv |> Array.toList) 1) |> getWeakness