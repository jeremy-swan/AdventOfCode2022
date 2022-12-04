open System
open System.IO

open Day4

let readFile = sprintf "input/%s" >> File.ReadAllText
let prepareFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq

let prepare = readFile >> prepareFile >> parseFile

let run part input func =
    input
    |> prepare
    |> func
    |> printfn "Part %i (TEST): %i" part
    printfn "==============================="


run 1 TEST_INPUT part1
run 1 REAL_INPUT part1
run 2 TEST_INPUT part2
run 2 REAL_INPUT part2