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