open System
open System.IO

open Day1

let readFile = sprintf "input/%s" >> File.ReadAllText
let parseFile (fileContent : string) = 
    fileContent
    |> fun x -> x.Split('\n')
    |> List.ofSeq
    |> List.map (fun x -> match Int32.TryParse(x) with
                          | true, n -> Some n
                          | _ -> None)

let prepare = readFile >> parseFile

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