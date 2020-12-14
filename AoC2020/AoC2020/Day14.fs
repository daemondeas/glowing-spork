module Day14

open System.Collections.Generic

    type Mask =
        { AndMask : int64
          OrMask  : int64 }

    type V2Mask =
        { OrMask : int64
          XBits  : int list }

    type InstructionType =
        | SetMask
        | MemWrite

    type Instruction =
        { Type    : InstructionType
          Mask    : string
          Address : int64
          Value   : int64 }

    type Computer =
        { Memory : Dictionary<int64, int64>
          Mask   : Mask }

    type V2Computer =
        { Memory : Dictionary<int64, int64>
          Masks  : V2Mask }

    let highestValue = 68719476735L

    let createMask =
        { AndMask = highestValue
          OrMask  = 0L }

    let createV2Mask =
        { OrMask = 0L
          XBits  = [] }

    let createComputer =
        { Memory = Dictionary<int64, int64>()
          Mask   = createMask }

    let createV2Computer =
        { Memory = Dictionary<int64, int64>()
          Masks  = createV2Mask }

    let parseInstruction (t: string) (v: string) =
        match t with
        | "mask" -> { Type    = SetMask
                      Mask    = v
                      Address = int64 0
                      Value   = int64 0 }
        | _      ->
            let indexA = t.IndexOf '[' + 1
            let indexB = t.IndexOf ']' - 1
            { Type    = MemWrite
              Address = int64 t.[indexA..indexB]
              Value   = int64 v
              Mask    = "" }

    let splitAndParse (s: string) =
        s.Split " = " |> fun a -> parseInstruction a.[0] a.[1]

    let rec getBits zeroes ones xes s =
        match s with
        | []    -> (zeroes, ones, xes)
        | c::cs ->
            match c with
            | 'X' -> getBits zeroes ones (cs.Length::xes) cs
            | '0' -> getBits (cs.Length::zeroes) ones xes cs
            | '1' -> getBits zeroes (cs.Length::ones) xes cs
            | _   -> failwith "Invalid mask character"

    let rec bitsToMask (current: int64) (bits: int list) =
        match bits with
        | []    -> current
        | b::bs -> bitsToMask (current ||| (1L <<< b)) bs

    let getMask (s: string) =
        Seq.toList s |> (getBits [] [] []) |> fun (z, o, _) ->
            { AndMask = ~~~(bitsToMask 0L z) &&& highestValue
              OrMask  = bitsToMask 0L o }

    let rec bitsToMasks (current: int64 list) (bits: int list) =
        match bits with
        | []    -> current
        | b::bs -> bitsToMasks (current@(List.map (fun i -> (1L <<< b) ||| i) current)) bs

    let getMasks (s: string) =
        let (_, o, x) = Seq.toList s |> (getBits [] [] [])
        let xMasks = bitsToMasks [0L] x
        { OrMask = bitsToMask 0L o
          XBits  = x }

    let setMask (c: Computer) m =
        { Memory = c.Memory
          Mask   = getMask m }

    let setMasks (c: V2Computer) m =
        { Memory = c.Memory
          Masks  = getMasks m }

    let rec getAddresses (bits: int list) (addresses: int64 list) =
        match bits with
        | []    -> List.distinct addresses
        | b::bs -> addresses@(List.map (fun a -> a ||| (1L <<< b)) addresses)@(List.map (fun a -> a &&& ~~~(1L <<< b)) addresses) |> getAddresses bs

    let rec writeToMemories (c: V2Computer) (value: int64) (addr: int64 list) =
        match addr with
        | []   -> c
        | a::s ->
            c.Memory.[a] <- value
            writeToMemories c value s

    let rec runProgram (inst: Instruction list) (c: Computer)  =
        match inst with
        | []    -> c
        | i::is ->
            match i.Type with
            | SetMask  -> setMask c i.Mask |> (runProgram is)
            | MemWrite ->
                c.Memory.[i.Address] <- i.Value &&& c.Mask.AndMask ||| c.Mask.OrMask
                runProgram is c

    let rec runProgramOnV2Computer (inst: Instruction list) (c: V2Computer) =
        match inst with
        | []    -> c
        | i::is ->
            match i.Type with
            | SetMask  -> setMasks c i.Mask |> (runProgramOnV2Computer is)
            | MemWrite -> getAddresses c.Masks.XBits [i.Address ||| c.Masks.OrMask] |> writeToMemories c i.Value |> (runProgramOnV2Computer is)

    let firstTask (argv: string[]) =
        Array.map splitAndParse argv |> Array.toList |> fun i -> runProgram i createComputer |> fun c -> Seq.reduce (+) c.Memory.Values

    let secondTask (argv: string[]) =
        Array.map splitAndParse argv |> Array.toList |> fun i -> runProgramOnV2Computer i createV2Computer |> fun c -> Seq.reduce (+) c.Memory.Values