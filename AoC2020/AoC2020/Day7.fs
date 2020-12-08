module Day7

open System.Collections.Generic

    type HeldBag =
        { Amount : int
          Name   : string }

    type Bag =
        { Colour : string
          Holds  : HeldBag list
          HeldBy : string[] }

    // vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
    // faded blue bags contain no other bags.

    let rec getContents (ss: string list) =
        match ss with
        | []    -> []
        | x::xs ->
            if x.StartsWith "no" then
                []
            else
                let space = x.IndexOf ' '
                let lastSpace = x.LastIndexOf ' '
                { Amount = int x.[..space - 1]
                  Name = x.[space + 1..lastSpace - 1] }::getContents xs

    let parseBag (s: string) =
        let parts = s.Split " bags contain "
        { Colour = parts.[0]
          Holds  = parts.[1].Split ',' |> Array.map (fun x -> x.Trim()) |> Array.toList |> getContents
          HeldBy = Array.empty }

    let rec mapHeldToDictionary (holding: string) (dictionary: Dictionary<string, string list>) (held: string list) =
        match held with
        | []    -> dictionary
        | h::hs ->
            if dictionary.ContainsKey h then
                dictionary.[h] <- holding::dictionary.[h]
            else
                dictionary.[h] <- [holding]
            mapHeldToDictionary holding dictionary hs

    let rec getHeldByDictionary (bags: Bag list) (dictionary: Dictionary<string, string list>) =
        match bags with
        | []    -> dictionary
        | b::bs -> List.map (fun hb -> hb.Name) b.Holds |> mapHeldToDictionary b.Colour dictionary |> getHeldByDictionary bs

    let rec getBagDictionary (bags: Bag list) (dictionary: Dictionary<string, Bag>) =
        match bags with
        | []    -> dictionary
        | b::bs ->
            dictionary.Add(b.Colour, b)
            getBagDictionary bs dictionary

    let rec getAllParentBags (dictionary: Dictionary<string, string list>) bag =
        if not (dictionary.ContainsKey bag) then
            []
        else
            let parents = dictionary.[bag]
            parents@(List.map (getAllParentBags dictionary) parents |> List.concat) |> List.distinct

    let rec getNumberOfChildBags (dictionary: Dictionary<string, Bag>) (bag: Bag) (factor: int) =
        if bag.Holds = [] then
            0
        else
            (List.map (fun h -> h.Amount * factor) bag.Holds |> List.reduce (+)) + (List.map (fun h -> (dictionary.[h.Name], h.Amount)) bag.Holds |> List.map (fun x -> getNumberOfChildBags dictionary (fst x) ((snd x) * factor)) |> List.reduce (+))

    let firstTask argv =
        let bags = Array.map parseBag argv |> Array.toList
        let dictionary = getHeldByDictionary bags (Dictionary<string, string list>())
        (getAllParentBags dictionary "shiny gold").Length

    let secondTask argv =
        let bags = Array.map parseBag argv |> Array.toList
        let dictionary = getBagDictionary bags (Dictionary<string, Bag>())
        getNumberOfChildBags dictionary dictionary.["shiny gold"] 1