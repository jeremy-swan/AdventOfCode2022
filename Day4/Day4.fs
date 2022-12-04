module Day4
open TypeExtensions
open System

let TEST_INPUT = "day4_test.txt"
let REAL_INPUT = "day4_real.txt"

type Range = int list
let buildRange (input : string) =
    input.Split('-')
    |> Array.map Int32.Parse
    |> function
       | [| s; e |] -> [ s..e ]
       | _ -> failwith "range must have exactly two elements"

let parseFile : string list -> (Range * Range) list = 
    List.map (fun line -> 
        line.Split(',')
        |> Array.map buildRange
        |> Array.toTupleOf2)


let part1 (input : (Range * Range) list ) = 
    input
    |> List.filter 
        (fun (r1, r2) -> (Set (r1 @ r2) |> Set.count) = (max r1.Length r2.Length))
    |> List.length

let part2 (input : (Range * Range) list) = 
    input
    |> List.filter 
        (fun (r1, r2) -> (Set (r1 @ r2) |> Set.count) <> (r1.Length + r2.Length))
    |> List.length