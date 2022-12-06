module Day6
open TypeExtensions
open System

let TEST_INPUT = "day6_test.txt"
let REAL_INPUT = "day6_real.txt"

let parseFile = id

let findDistinctCharacters count =
    (Seq.toList
    >> List.windowed count
    >> List.findIndex (fun chars -> (Set chars).Count = count)
    >> fun i -> i + count)

let part1 (input : string list) = 
    input 
    |> List.map (findDistinctCharacters 4)

let part2 (input : string list) = 
    input 
    |> List.map (findDistinctCharacters 14)