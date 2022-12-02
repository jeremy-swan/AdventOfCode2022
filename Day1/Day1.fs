module Day1 
open TypeExtensions
open System

let TEST_INPUT = "day1_test.txt"
let REAL_INPUT = "day1_real.txt"

let parseFile : string list -> int option list =
    List.map (fun x -> match Int32.TryParse(x) with
                        | true, n -> Some n
                        | _ -> None)


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