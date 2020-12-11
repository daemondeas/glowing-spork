module Day10
    let rec getJoltageCounts currentJoltage ones threes adapters =
        match adapters with
        | []    -> (ones, threes + 1)
        | j::js ->
            match j - currentJoltage with
            | 1 -> getJoltageCounts j (ones + 1) threes js
            | 3 -> getJoltageCounts j ones (threes + 1) js
            | _ -> getJoltageCounts j ones threes js

    let rec completeJoltage current ones twos threes adapters =
        match adapters with
        | []    -> (ones, twos, threes + 1)
        | j::js ->
            match j - current with
            | 1 -> completeJoltage j (ones + 1) twos threes js
            | 2 -> completeJoltage j ones (twos + 1) threes js
            | 3 -> completeJoltage j ones twos (threes + 1) js
            | _ -> completeJoltage j ones twos threes js

    let rec getNumberOfWaysToNodes from nodes =
        match nodes with
        | []    -> 1
        | n::ns ->
            if n - from > 3 then
                0
            else
                1 + getNumberOfWaysToNodes from ns

    let rec getNumberOfWaysThrough from waysHere nodes =
        match nodes with
        | []    -> 1
        | n::ns ->
            if n - from > 3 then
                0
            else
                waysHere + (getNumberOfWaysThrough from waysHere ns) + (getNumberOfWaysThrough n waysHere ns)

    let rec getMatchingIndices value nodes matches current =
        match nodes with
        | []    -> matches
        | n::ns ->
            if n - value > 3 then
                matches
            else
                getMatchingIndices value ns (current::matches) (current + 1)

    let rec countWaysThrough index (ways: int64[]) (nodes: int list) =
        match index with
        | -1 -> ways.[0]
        | i  ->
            let indices = getMatchingIndices nodes.[i] nodes.[i + 1..i + 4] [] 1
            ways.[i] <- (List.map (fun j -> ways.[j]) indices |> List.fold (+) (int64 0))
            countWaysThrough (index - 1) ways nodes

    let firstTask (argv: string[]) =
        Array.map int argv |> Array.sort |> Array.toList |> (getJoltageCounts 0 0 0) |> (fun (a, b) -> a * b)

    let secondTask (argv: string[]) =
        let ways = Array.zeroCreate (argv.Length + 1) |> Array.map int64
        ways.[argv.Length] <- (int64 1)
        ways.[argv.Length - 1] <- (int64 1)
        Array.map int argv |> Array.sort |> Array.toList |> (fun a -> [0]@a) |> (countWaysThrough (argv.Length - 2) ways)