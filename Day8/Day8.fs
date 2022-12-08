module Day8
open TypeExtensions
open System

let TEST_INPUT = "day8_test.txt"
let REAL_INPUT = "day8_real.txt"

type Grid = int list list
type VisibilityGrid = int option list list
let toVisibilityGrid = List.map (List.map Some)
 
let parseFile (input: string list) : Grid = 
    input
    |> List.map 
        (fun line -> 
            line 
            |> List.ofSeq
            |> List.map (string >> Int32.Parse))

let zipOptions (l1 : 'a option list) (l2 : 'a option list) =
    List.zip l1 l2
    |> List.map (function
                 | Some f, Some _ -> Some f
                 | Some f, None -> Some f
                 | None, Some b -> Some b
                 | None, None -> None)


type VisibilityCheckAggregator = {
    items : int option list
    maxHeight : int
}
module VisibilityCheckAggregator =
    let empty = {
        items = []
        maxHeight = -1
    }
let checkIsVisible (input : int option list) = 
    let check = 
        fun (agg : VisibilityCheckAggregator) item ->
            match item with
            | Some height when height > agg.maxHeight -> { agg with items = agg.items@[Some height]
                                                                    maxHeight = height } 
            | _ -> { agg with items = agg.items@[None] }

    let forwards = 
        input
        |> List.fold check VisibilityCheckAggregator.empty
        |> fun agg -> agg.items
    let backwards = 
        input
        |> List.rev
        |> List.fold check VisibilityCheckAggregator.empty
        |> fun agg -> agg.items
        |> List.rev

    zipOptions forwards backwards


let getVisibleFromEdge (input : VisibilityGrid) =
    let rows = 
        input
        |> List.map checkIsVisible

    let columns = 
        input
        |> List.asTransposed (List.map checkIsVisible)

    List.zip rows columns
    |> List.map (fun (rowCheck, colCheck) -> zipOptions rowCheck colCheck)

let part1 (input : Grid) = 
    input
    |> toVisibilityGrid
    |> getVisibleFromEdge
    |> List.sumBy (List.choose id >> List.length)



// ===============  PART 2 ================

type ScenicCheckAggregator = {
    items : int list
    scores : int list
}
module ScenicCheckAggregator = 
    let empty = {
        items = []
        scores = []
    }
let getVisibleFromEachTree (input : int list) =
    let getScore = fun (agg : ScenicCheckAggregator) curr -> 
        let score = 
            agg.items
            |> List.takeWhile (fun item -> item < curr)
            |> fun visible -> 
                if List.length visible = List.length agg.items 
                then List.length visible
                else List.length visible + 1
        
        { agg with items = curr::agg.items
                   scores = agg.scores@[score]}
    

    let forwards =
        input
        |> List.fold getScore ScenicCheckAggregator.empty
        |> fun agg -> agg.scores

    let backwards = 
        input 
        |> List.rev
        |> List.fold getScore ScenicCheckAggregator.empty
        |> fun agg -> agg.scores
        |> List.rev

    List.zip forwards backwards
    |> List.map (fun (f, b) -> f * b)

let getScenicScores (input : int list list) =
    let rows = 
        input
        |> List.map getVisibleFromEachTree

    let columns = 
        input
        |> List.asTransposed (List.map getVisibleFromEachTree)

    List.zip rows columns
    |> List.map (fun (rowScores, colScores) ->
        List.zip rowScores colScores
        |> List.map (fun (r, c) -> r * c))


let part2 (input : Grid) =
    getScenicScores input
    |> List.map List.max
    |> List.max