module Day1 
open TypeExtensions

let TEST_INPUT = "day1_test.txt"
let REAL_INPUT = "day1_real.txt"


let part1 (input : int option list) =
    input
    |> List.splitBy Option.isNone
    |> List.map (List.choose id >> List.sum)
    |> List.max

let part2 (input : int option list) =
    input
    |> List.splitBy Option.isNone
    |> List.map (List.choose id >> List.sum)
    |> List.sortDescending
    |> List.take 3
    |> List.sum