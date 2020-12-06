module Day6
    let differentAnswers s =
        let answers = Seq.distinct s |> Seq.toArray
        answers.Length

    let joinGroup (s:string) =
        s.Split '\n' |> Seq.concat

    let countGroupAnswers s =
        joinGroup s |> differentAnswers

    let sameAnswers (s:string) =
        let answers = s.Split '\n' |> Seq.map Set.ofSeq |> Seq.reduce Set.intersect
        answers.Count

    let firstTask argv =
        Array.map countGroupAnswers argv |> Array.reduce (+)

    let secondTask argv =
        Array.map sameAnswers argv |> Array.reduce (+)