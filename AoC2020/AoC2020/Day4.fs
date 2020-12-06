module Day4
    type Property =
        { Name  : string
          Value : string }

    type Passport =
        { Properties : Property[] }

    type Length =
        { Unit  : string
          Value : int }

    let requiredFields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

    let eyeColours = ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

    let rec xsContainsYs xs ys =
        match ys with
        | []    -> true
        | z::zs -> Seq.contains z xs && xsContainsYs xs zs

    let isValid (passPort: Passport) =
        xsContainsYs (Seq.map (fun (p:Property) -> p.Name) passPort.Properties) requiredFields

    let createProperty (s: string) =
        let parts = s.Split ':'
        { Name  = parts.[0]
          Value = parts.[1] }

    let createPassport (props: string[]) =
        { Properties = Array.map createProperty props }

    let splitToPropertyStrings (s: string) =
        s.Split (List.toArray [' '; '\n'])

    let rec createPassports passports =
        match passports with
        | []      -> []
        | pp::pps -> createPassport (splitToPropertyStrings pp)::createPassports pps

    let getLength (s: string) =
        let split = s.Length - 2
        if s.Contains "cm" || s.Contains "in" then
            { Unit  = s.[split..]
              Value = int s.[..split - 1]}
        else
            { Unit  = "Unknown"
              Value = -1 }

    let isValidLength (l:Length) =
        match l.Unit with
        | "cm" -> l.Value >= 150 && l.Value <= 193
        | "in" -> l.Value >= 59  && l.Value <= 76
        | _    -> false

    let rec isHexString s =
        match s with
        | []    -> true
        | x::xs -> (x >= '0' && x <= '9' || x >= 'a' && x <= 'f') && isHexString xs

    let isValidHairColour (s: string) =
        s.Length = 7 && s.[0] = '#' && Seq.toList s.[1..] |> isHexString

    let isValidProperty (property: Property) =
        match property.Name with
        | "byr" -> property.Value.Length = 4 && (int property.Value >= 1920) && (int property.Value <= 2002)
        | "iyr" -> property.Value.Length = 4 && (int property.Value >= 2010) && (int property.Value <= 2020)
        | "eyr" -> property.Value.Length = 4 && (int property.Value >= 2020) && (int property.Value <= 2030)
        | "hgt" -> getLength property.Value |> isValidLength
        | "hcl" -> isValidHairColour property.Value
        | "ecl" -> List.contains property.Value eyeColours
        | "pid" -> property.Value.Length = 9 && System.Int64.TryParse property.Value |> fst
        | _     -> true

    let isFullyValidPassport (passPort: Passport) =
        isValid passPort && Array.map isValidProperty passPort.Properties |> Array.reduce (&&)

    let firstTask argv =
        let passports = createPassports argv
        Common.count true (List.map isValid passports)

    let secondTask argv =
        let passports = createPassports argv
        Common.count true (List.map isFullyValidPassport passports)