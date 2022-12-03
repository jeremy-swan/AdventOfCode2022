module Day3
open TypeExtensions
open System

let TEST_INPUT = "day3_test.txt"
let REAL_INPUT = "day3_real.txt"

type RucksackSection = char list
type Rucksack = RucksackSection * RucksackSection

let parseFile : string list -> Rucksack list = 
    List.map (Seq.toList 
              >> List.splitInto 2
              >> List.toTupleOf2)

let priority (c : char) = //int c - int 'a' + 1
    match c with
    | c when c >= 'a' -> int c - int 'a' + 1
    | _ -> int c - int 'A' + 27

let part1 (input : Rucksack list) =
    input
    |> List.map (fun (s1, s2) -> (List.intersect s1 s2) |> List.head)
    |> List.sumBy priority

let part2 (input : Rucksack list) =
    input
    |> List.splitInto (List.length input / 3)
    |> List.map (List.map (fun (s1, s2) -> s1 @ s2)
                 >> List.reduce (List.intersect) 
                 >> List.head)
    |> List.sumBy priority