open System
open System.IO

open Day8

let readFile = sprintf "input/%s" >> File.ReadAllText
let prepareFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq

let prepare = readFile >> prepareFile >> parseFile

let run part stage input func =
    input
    |> prepare
    |> func
    |> printfn "Part %i (%s): %O" part stage
    printfn "==============================="


run 1 "TEST" TEST_INPUT part1
run 1 "REAL" REAL_INPUT part1
run 2 "TEST" TEST_INPUT part2
run 2 "REAL" REAL_INPUT part2