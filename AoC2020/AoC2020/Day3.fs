module Day3
    type Spot =
        | Open
        | Tree

    type Forest =
        { Width  : int
          Height : int
          Rows   : Spot[][] }

    let getSpot (forest: Forest) y x =
        if y >= forest.Height then
            Open
        else
            forest.Rows.[y].[x % forest.Width]

    let rec traverseForest (forest: Forest) y x stepY stepX =
        if y >= forest.Height then
            []
        else
            getSpot forest y x::(traverseForest forest (y + stepY) (x + stepX) stepY stepX)

    let createSpot c =
        if c = '.' then
            Open
        else
            Tree

    let rec createSpots row =
        match row with
        | []    -> []
        | x::xs -> createSpot x::createSpots xs

    let rec createForest rows =
        match rows with
        | []    -> []
        | x::xs -> createSpots x::createForest xs

    let getForest (xs: string[]) =
        {
            Width  = xs.[0].Length
            Height = xs.Length
            Rows   = List.toArray (List.map List.toArray (createForest (List.map Seq.toList (Array.toList xs))))
        }

    let firstTask argv =
        let forest = getForest argv
        traverseForest forest 0 0 1 3 |> (Common.count Tree)

    let secondTask argv =
        let forest = getForest argv
        (traverseForest forest 0 0 1 1 |> (Common.count Tree)) * (traverseForest forest 0 0 1 3 |> (Common.count Tree)) * (traverseForest forest 0 0 1 5 |> (Common.count Tree)) * (traverseForest forest 0 0 1 7 |> (Common.count Tree)) * (traverseForest forest 0 0 2 1 |> (Common.count Tree))