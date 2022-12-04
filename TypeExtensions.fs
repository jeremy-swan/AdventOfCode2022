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
        |> fun (x, _, _) -> x
        |> List.rev

    let toTupleOf2 = function
        | [x1; x2] -> (x1, x2)
        | _ -> failwith "input list can only have two items"

    let intersect l1 l2 =
        Set.intersect (Set.ofList l1) (Set.ofList l2)
        |> Set.toList

module Array =
    let toTupleOf2 = function
        | [|x1; x2|] -> (x1, x2)
        | _ -> failwith "input list can only have two items"