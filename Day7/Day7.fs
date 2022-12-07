module Day7
open TypeExtensions
open System
open System.Collections.Generic

let TEST_INPUT = "day7_test.txt"
let REAL_INPUT = "day7_real.txt"

let PATH_SEPARATOR = " "

type File = {
    path : string
    name : string
    size : int
}
type DirectorySize = int
let getDirectorySizes (files : File list) : DirectorySize list =
    files
    |> List.groupBy (fun f -> f.path)
    |> List.map fst
    |> List.collect (fun p -> 
        p.Split(PATH_SEPARATOR)
        |> Array.fold (fun agg curr -> ((List.head agg) + PATH_SEPARATOR + curr)::agg) [""])
        |> List.map (fun x -> x.Trim()) // generate path entries for directories with no files
    |> List.distinct
    |> List.map (fun p -> 
        files
        |> List.filter (fun f -> f.path.StartsWith(p))
        |> List.sumBy (fun f -> f.size))

type Aggregator = {
    currentPath : string list
    files : File list
}
module Aggregator =
    let empty = {
        currentPath = []
        files = []
    }
let parseFile (input : string list) =
    input
    |> List.fold (fun (agg : Aggregator) (curr : string) -> 
        match curr.Split(" ") with 
        | [| "$"; "cd"; ".." |] -> { agg with currentPath = agg.currentPath |> List.removeLast }
        | [| "$"; "cd"; dir |] -> { agg with currentPath = agg.currentPath @ [dir] }
        | [| "$"; _ |]
        | [| "dir"; _ |] -> agg
        | [| fileSize; fileName |] -> { 
                agg with files = agg.files @ [{ 
                        path = agg.currentPath |> String.concat PATH_SEPARATOR
                        name = fileName
                        size = Int32.Parse fileSize 
                }]}
        | _ -> agg) Aggregator.empty
    |> fun x -> x.files

let part1 (input : File list) =
    input 
    |> getDirectorySizes
    |> List.filter (fun s -> s <= 100000)
    |> List.sum

let part2 (input : File list) = 
    let directorySizes = input |> getDirectorySizes
    let rootSize = directorySizes.[0]

    let usedSpace = 70000000 - rootSize

    directorySizes
    |> List.filter (fun s -> usedSpace + s >= 30000000)
    |> List.sort
    |> List.head