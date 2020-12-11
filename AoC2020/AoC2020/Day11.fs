module Day11
    type Tile =
        | Floor
        | Empty
        | Occupied

    type Grid =
        { Tiles  : Tile list list
          Width  : int
          Height : int }

    let directions = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]

    let getTile x y (grid: Grid) =
        if x < 0 || y < 0 || x >= grid.Width || y >= grid.Height then
            Floor
        else
            grid.Tiles.[y].[x]

    let parseTile c =
        match c with
        | '.' -> Floor
        | 'L' -> Empty
        | '#' -> Occupied
        | _   -> failwith "fake tile"

    let createGrid (argv: string list) =
        { Height = argv.Length
          Width  = argv.[0].Length
          Tiles  = List.map (fun s -> Seq.toList s |> List.map parseTile) argv }

    let rec getSurroundingInnerVal x ys valX valY =
        match ys with
        | []    -> []
        | b::bs ->
            if x = valX && b = valY then
                getSurroundingInnerVal x bs valX valY
            else
                (x, b)::getSurroundingInnerVal x bs valX valY

    let rec getSurroundingInner xs ys valX valY =
        match xs with
        | []    -> []
        | c::cs -> getSurroundingInnerVal c ys valX valY@getSurroundingInner cs ys valX valY

    let getSurroundingTiles x y (grid: Grid) =
        getSurroundingInner [x - 1..x + 1] [y - 1..y + 1] x y |> List.map (fun (a, b) -> getTile a b grid)

    let rec getSightedTiles x y cX cY grid =
        if x < 0 || y < 0 || x >= grid.Width || y >= grid.Height then
            Floor
        else
            let tile = getTile x y grid
            match tile with
            | Occupied -> Occupied
            | Empty    -> Empty
            | _        -> getSightedTiles (x + cX) (y + cY) cX cY grid

    let getSurroundingSighted x y grid =
        List.map (fun (cx, cy) -> getSightedTiles (x + cx) (y + cy) cx cy grid) directions

    let stepTile limit tile adjacentOccupied =
        match tile with
        | Floor    -> Floor
        | Empty    ->
            if adjacentOccupied = 0 then
                Occupied
            else
                Empty
        | Occupied ->
            if adjacentOccupied >= limit then
                Empty
            else
                Occupied

    let rec stepTilesInner y x grid (stepFunction: Tile -> int -> Tile) (getSurroundingFunction: int -> int -> Grid -> Tile list) =
        match grid.Width - x with
        | 0 -> []
        | _ -> stepFunction (getTile x y grid) (getSurroundingFunction x y grid |> Common.count Occupied)::stepTilesInner y (x + 1) grid stepFunction getSurroundingFunction

    let rec steptiles y grid (stepFunction: Tile -> int -> Tile) (getSurroundingFunction: int -> int -> Grid -> Tile list) =
        match grid.Height - y with
        | 0 -> []
        | _ -> stepTilesInner y 0 grid stepFunction getSurroundingFunction::steptiles (y + 1) grid stepFunction getSurroundingFunction

    let stepGrid (grid: Grid) (stepFunction: Tile -> int -> Tile) (getSurroundingFunction: int -> int -> Grid -> Tile list) =
        { Width  = grid.Width
          Height = grid.Height
          Tiles  = steptiles 0 grid stepFunction getSurroundingFunction }

    let sameTile (a: Grid) (b: Grid) x y =
        a.Tiles.[y].[x] = b.Tiles.[y].[x]

    let rec sameRow (a: Grid) (b: Grid) y x =
        match a.Width - x with
        | 0 -> true
        | _ -> sameTile a b x y && sameRow a b y (x + 1)

    let rec sameTiles (a: Grid) (b: Grid) y =
        match a.Height - y with
        | 0 -> true
        | _ -> sameRow a b y 0 && sameTiles a b (y + 1)

    let areEqual (a: Grid) (b:Grid) =
        a.Height = b.Height && a.Width = b.Width && sameTiles a b 0

    let rec stepGridUntilStable (stepFunction: Tile -> int -> Tile) (getSurroundingFunction: int -> int -> Grid -> Tile list) (grid: Grid) =
        let newGrid = stepGrid grid stepFunction getSurroundingFunction
        if areEqual grid newGrid then
            newGrid
        else
            stepGridUntilStable stepFunction getSurroundingFunction newGrid

    let getOccupied (grid: Grid) =
        List.map (Common.count Occupied) grid.Tiles |> List.reduce (+)

    let firstTask argv =
        Array.toList argv |> createGrid |> (stepGridUntilStable (stepTile 4) getSurroundingTiles) |> getOccupied

    let secondTask argv =
        Array.toList argv |> createGrid |> (stepGridUntilStable (stepTile 5) getSurroundingSighted) |> getOccupied