module Day2
    type PasswordPolicy =
        { Character : char
          Minimum   : int
          Maximum   : int }

    type PasswordPolicyCombination =
        { Policy   : PasswordPolicy
          Password : string }

    let isValid (ppc:PasswordPolicyCombination) =
        let amount = Common.count ppc.Policy.Character ppc.Password
        ppc.Policy.Minimum <= amount && ppc.Policy.Maximum >= amount

    let isValidToboggan (ppc:PasswordPolicyCombination) =
        (ppc.Password.[ppc.Policy.Minimum - 1] = ppc.Policy.Character) <> (ppc.Password.[ppc.Policy.Maximum - 1] = ppc.Policy.Character)

    let createCombinationFromStringArray (xs: string[]) =
        let boundaries = xs.[0].Split '-'
        {
            Policy = {
                Minimum   = int boundaries.[0]
                Maximum   = int boundaries.[1]
                Character = xs.[1].[0]}
            Password = xs.[2]}

    let countValid (ppcs: string[]) (valid: PasswordPolicyCombination -> bool) =
        Array.chunkBySize 3 ppcs
        |> Array.map createCombinationFromStringArray
        |> Array.map valid
        |> Common.count true

    let firstTask (ppcs:string[]) =
        countValid ppcs isValid

    let secondTask (ppcs:string[]) =
        countValid ppcs isValidToboggan