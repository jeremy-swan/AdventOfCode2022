module TypeExtensions

module List =
    // splits a list by a predicate, removing the bounding item
    let splitBy (predicate: 'a -> bool) (list: 'a list) =
        list
        |> List.fold
            (fun ((agg, curr, i)) (item: 'a) -> 
                match predicate item, 
                      i = List.length list - 1 with
                | false, false -> (agg, item::curr, i + 1)
                | false, true -> ((item::curr)::agg, [], i + 1)
                | true, _ -> (curr::agg, [], i + 1))
            ([], [], 0)
        |> fun (x, _, _) -> (x |> List.map List.rev)
        |> List.rev

    let toTupleOf2 = function
        | [x1; x2] -> (x1, x2)
        | _ -> failwith "input list can only have two items"

    let intersect l1 l2 =
        Set.intersect (Set.ofList l1) (Set.ofList l2)
        |> Set.toList

    // diagonally inverts a list
    let inverse (list : 'a list list) = 
        let xLength = List.head list |> List.length
        let yLength = list |> List.length

        [0..xLength-1]
        |> List.map(fun xI -> 
            [0..yLength-1]
            |> List.map(fun yI -> 
                list[yI][xI]))

    // turns the rows into columns and vice versa
    let transpose (list : 'a list list) =
        let xLength = List.head list |> List.length
        let yLength = list |> List.length

        [0..xLength-1]
        |> List.map(fun xI -> 
            [0..yLength-1]
            |> List.map(fun yI -> 
                list[yLength - yI - 1][xI])
            |> List.rev)

    let removeLast list = list |> List.removeAt (List.length list - 1)

module Array =
    let toTupleOf2 = function
        | [|x1; x2|] -> (x1, x2)
        | _ -> failwith "input list can only have two items"