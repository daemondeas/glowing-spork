module Day8
    type OperationType =
        | Nop
        | Acc
        | Jmp

    type Instruction =
        { Operation : OperationType
          Value     : int
          RunTimes  : int }

    type Computer =
        { Acc : int
          PC  : int }

    let parseOperation o =
        match o with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp

    let parseInstruction (s: string) =
        let parts = s.Split ' '
        { Operation = parseOperation parts.[0]
          Value     = int parts.[1]
          RunTimes  = 0 }

    let rec runProgramOnComputerUntilRepeat (c: Computer) (instr: Instruction[]) =
        match instr.[c.PC].RunTimes with
        | 1 -> c.Acc
        | _ ->
            instr.[c.PC] <- { Operation = instr.[c.PC].Operation
                              Value     = instr.[c.PC].Value
                              RunTimes  = instr.[c.PC].RunTimes + 1 }
            match instr.[c.PC].Operation with
            | Nop -> runProgramOnComputerUntilRepeat { Acc = c.Acc
                                                       PC  = c.PC + 1 } instr
            | Jmp -> runProgramOnComputerUntilRepeat { Acc = c.Acc
                                                       PC  = c.PC + instr.[c.PC].Value } instr
            | Acc -> runProgramOnComputerUntilRepeat { Acc = c.Acc + instr.[c.PC].Value
                                                       PC  = c.PC + 1 } instr

    let rec runProgramOnComputerUntilFinish  (c: Computer) (instr: Instruction[]) =
        if c.PC = instr.Length then
            Some c.Acc
        else if c.PC > instr.Length then
            None
        else
            match instr.[c.PC].RunTimes with
            | 1 -> None
            | _ ->
                instr.[c.PC] <- { Operation = instr.[c.PC].Operation
                                  Value     = instr.[c.PC].Value
                                  RunTimes  = instr.[c.PC].RunTimes + 1 }
                match instr.[c.PC].Operation with
                | Nop -> runProgramOnComputerUntilFinish { Acc = c.Acc
                                                           PC  = c.PC + 1 } instr
                | Jmp -> runProgramOnComputerUntilFinish { Acc = c.Acc
                                                           PC  = c.PC + instr.[c.PC].Value } instr
                | Acc -> runProgramOnComputerUntilFinish { Acc = c.Acc + instr.[c.PC].Value
                                                           PC  = c.PC + 1 } instr

    let rec tryModifications index (instr: Instruction[]) =
        match instr.[index].Operation with
        | Acc -> tryModifications (index + 1) instr
        | Jmp ->
            let copied = Array.copy instr
            copied.[index] <- { Operation = Nop
                                Value     = instr.[index].Value
                                RunTimes  = 0 }
            let result = runProgramOnComputerUntilFinish { Acc = 0
                                                           PC  = 0 } copied
            match result with
            | Some i -> i
            | None   -> tryModifications (index + 1) instr
        | Nop ->
            let copied = Array.copy instr
            copied.[index] <- { Operation = Jmp
                                Value     = instr.[index].Value
                                RunTimes  = 0 }
            let result = runProgramOnComputerUntilFinish { Acc = 0
                                                           PC  = 0 } copied
            match result with
            | Some i -> i
            | None   -> tryModifications (index + 1) instr

    let firstTask argv =
        Array.map parseInstruction argv |> runProgramOnComputerUntilRepeat { Acc = 0
                                                                             PC  = 0 }

    let secondTask argv =
        Array.map parseInstruction argv |> tryModifications 0