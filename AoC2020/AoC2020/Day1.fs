module Day1
    let rec findPairInner x xs sum =
        match xs with
        | []    -> (0, 0)
        | y::ys ->
            if x + y = sum then
                (x, y)
            else
                findPairInner x ys sum

    let rec findPair xs sum =
        match xs with
        | []    -> (0, 0)
        | y::ys ->
            let (a, b) = findPairInner y ys sum
            if (a, b) = (0, 0) then
                findPair ys sum
            else
                (a, b)

    let firstTask xs =
        let (a, b) = findPair xs 2020
        a * b

    let rec findTripleInner a b xs sum =
        match xs with
        | []    -> (0, 0, 0)
        | y::ys ->
            if a + b + y = sum then
                (a, b, y)
            else
                findTripleInner a b ys sum

    let rec fti x xs sum =
        match xs with
        | []    -> (0, 0, 0)
        | y::ys ->
            let (a, b, c) = findTripleInner x y ys sum
            if (a, b, c) = (0, 0, 0) then
                fti x ys sum
            else
                (a, b, c)

    let rec findTriple xs sum =
        match xs with
        | []    -> (0, 0, 0)
        | y::ys ->
            let (a, b, c) = fti y ys sum
            if (a, b, c) = (0, 0, 0) then
                findTriple (ys) sum
            else
                (a, b, c)

    let secondTask xs =
        let (a, b, c) = findTriple xs 2020
        a * b * c