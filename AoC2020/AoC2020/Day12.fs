module Day12
    type Direction =
        | North
        | South
        | East
        | West
        | Left
        | Right
        | Forward

    type Instruction =
        { Direction : Direction
          Value     : int }

    type Ship =
        { Direction : Direction
          XPosition : int
          YPosition : int }

    type Waypoint =
        { PosX : Instruction
          PosY : Instruction }

    let parseDirection c =
        match c with
        | 'N' -> North
        | 'S' -> South
        | 'E' -> East
        | 'W' -> West
        | 'L' -> Left
        | 'R' -> Right
        | 'F' -> Forward
        | _   -> failwith "Bad direction"

    let ParseInstruction (s: string) =
        { Direction = parseDirection s.[0]
          Value     = int s.[1..] }

    let createShip =
        { Direction = East
          XPosition = 0
          YPosition = 0 }

    let createInitialWaypoint =
        { PosX = { Direction = East
                   Value     = 10 }
          PosY = { Direction = North
                   Value     = 1 }}

    let directionAsNumber direction =
        match direction with
        | East  -> 0
        | South -> 1
        | West  -> 2
        | North -> 3
        | _     -> 0

    let rec numberAsDirection number =
        match number % 4 with
        | 0 -> East
        | 1 -> South
        | 2 -> West
        | 3 -> North
        | n -> numberAsDirection (n + 4)

    let turnQuarters direction quarters =
        directionAsNumber direction + quarters |> numberAsDirection

    let turn direction (instruction: Instruction) =
        match instruction.Direction with
        | Left  -> turnQuarters direction -(instruction.Value / 90)
        | Right -> turnQuarters direction (instruction.Value / 90)
        | _     -> direction

    let isYDirection direction =
        direction = North || direction = South

    let fixDirections (wayPoint: Waypoint) =
        match isYDirection wayPoint.PosY.Direction with
        | true  -> wayPoint
        | false -> { PosX = wayPoint.PosY
                     PosY = wayPoint.PosX }

    let turnWaypoint (waypoint: Waypoint) (instruction: Instruction) =
        { PosX = { Direction = turn waypoint.PosX.Direction instruction
                   Value     = waypoint.PosX.Value }
          PosY = { Direction = turn waypoint.PosY.Direction instruction
                   Value     = waypoint.PosY.Value }} |> fixDirections

    let rec moveShip (ship: Ship) (instruction: Instruction) =
        match instruction.Direction with
        | Forward -> moveShip ship { Direction = ship.Direction
                                     Value     = instruction.Value }
        | East    -> { Direction = ship.Direction
                       XPosition = ship.XPosition + instruction.Value
                       YPosition = ship.YPosition }
        | South   -> { Direction = ship.Direction
                       XPosition = ship.XPosition
                       YPosition = ship.YPosition - instruction.Value }
        | West    -> { Direction = ship.Direction
                       XPosition = ship.XPosition - instruction.Value
                       YPosition = ship.YPosition }
        | North   -> { Direction = ship.Direction
                       XPosition = ship.XPosition
                       YPosition = ship.YPosition + instruction.Value }
        | _       -> { Direction = turn ship.Direction instruction
                       XPosition = ship.XPosition
                       YPosition = ship.YPosition }

    let rec runShipAccordingToInstructions instructions ship =
        match instructions with
        | []    -> ship
        | i::is -> moveShip ship i |> runShipAccordingToInstructions is

    let getOppositeDirection direction =
        match direction with
        | East  -> West
        | West  -> East
        | North -> South
        | South -> North
        | _     -> direction

    let fixPosition (position: Instruction) =
        if position.Value < 0 then
            { Direction = getOppositeDirection position.Direction
              Value     = -position.Value }
        else
            position

    let movePosition (position: Instruction) (instruction: Instruction) =
        match position.Direction = instruction.Direction with
        | true  -> { Direction = position.Direction
                     Value     = position.Value + instruction.Value }
        | false -> { Direction = position.Direction
                     Value     = position.Value - instruction.Value } |> fixPosition

    let moveWaypoint (wayPoint: Waypoint) (instruction: Instruction) =
        if isYDirection instruction.Direction then
            { PosX = wayPoint.PosX
              PosY = movePosition wayPoint.PosY instruction }
        else
            { PosX = movePosition wayPoint.PosX instruction
              PosY = wayPoint.PosY }

    let getWaypointInstructions (waypoint: Waypoint) value =
        ({ Direction = waypoint.PosX.Direction
           Value     = waypoint.PosX.Value * value},
         { Direction = waypoint.PosY.Direction
           Value     = waypoint.PosY.Value * value })

    let moveShipTowardsWaypoint ship (waypoint: Waypoint) value =
        let (firstInstruction, secondInstruction) = getWaypointInstructions waypoint value
        let intermediateShip = moveShip ship firstInstruction
        moveShip intermediateShip secondInstruction

    let moveShipWithWaypoint ship waypoint (instruction: Instruction) =
        match instruction.Direction with
        | North   -> (ship, moveWaypoint waypoint instruction)
        | South   -> (ship, moveWaypoint waypoint instruction)
        | East    -> (ship, moveWaypoint waypoint instruction)
        | West    -> (ship, moveWaypoint waypoint instruction)
        | Left    -> (ship, turnWaypoint waypoint instruction)
        | Right   -> (ship, turnWaypoint waypoint instruction)
        | Forward -> (moveShipTowardsWaypoint ship waypoint instruction.Value, waypoint)

    let rec runShipAccordingToInstructionsAndWaypoint instructions waypoint ship =
        match instructions with
        | []    -> ship
        | i::is -> moveShipWithWaypoint ship waypoint i |> (fun (s, w) -> runShipAccordingToInstructionsAndWaypoint is w s)

    let abs i =
        if i < 0 then
            -i
        else
            i

    let getHamiltonianDistance aX aY bX bY =
        (abs (aX - bX)) + (abs (aY - bY))

    let firstTask argv =
        Array.map ParseInstruction argv |> Array.toList |> (fun is -> runShipAccordingToInstructions is createShip) |> (fun s -> getHamiltonianDistance 0 0 s.XPosition s.YPosition)

    let secondTask argv =
        Array.map ParseInstruction argv |> Array.toList |> (fun is -> runShipAccordingToInstructionsAndWaypoint is createInitialWaypoint createShip) |> (fun s -> getHamiltonianDistance 0 0 s.XPosition s.YPosition)