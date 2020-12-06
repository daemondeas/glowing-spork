module Day5
    type Boundaries =
        { Lower  : int
          Higher : int }

    let rows =
        { Lower  = 0
          Higher = 127 }

    let columns =
        { Lower  = 0
          Higher = 7 }

    let chooseLowerHalf (b:Boundaries) =
        { Lower  = b.Lower
          Higher = b.Lower + (b.Higher - b.Lower) / 2 }

    let chooseUpperHalf (b:Boundaries) =
        { Lower  = b.Lower + (b.Higher - b.Lower) / 2 + 1
          Higher = b.Higher }

    let chooseHalf (b: Boundaries) c =
        match c with
        | 'F' -> chooseLowerHalf b
        | 'B' -> chooseUpperHalf b
        | 'L' -> chooseLowerHalf b
        | 'R' -> chooseUpperHalf b

    let rec partitionBoundaries (b: Boundaries) p =
        match p with
        | []    -> b.Lower
        | x::xs -> partitionBoundaries (chooseHalf b x) xs

    let getRow s =
        Seq.toList s |> partitionBoundaries rows

    let getColumn s =
        Seq.toList s |> partitionBoundaries columns

    let getId (s:string) =
        let row = getRow s.[..6]
        let col = getColumn s.[7..]
        row * 8 + col

    let rec findMissing fullList lookingIn =
        match fullList with
        | []    -> 0
        | x::xs -> if Seq.contains x lookingIn then findMissing xs lookingIn else x

    let firstTask argv =
        Array.map getId argv |> Array.reduce (fun a b -> if a > b then a else b)

    let secondTask argv =
        let ids = Array.map getId argv |> Array.sort
        let lowest = ids.[0]
        let highest = ids.[ids.Length - 1]
        let fullSpace = [lowest..highest]
        findMissing fullSpace ids