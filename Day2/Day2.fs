module Day2
open TypeExtensions

let TEST_INPUT = "day2_test.txt"
let REAL_INPUT = "day2_real.txt"

type Choice = Rock | Paper | Scissors
let toOpponentChoice = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "invalid choice"

let toMyChoice = function
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "invalid choice"

type Outcome = Win | Draw | Loss

let WinScore = 6
let DrawScore = 3
let LossScore = 0
let getChoiceScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getRoundOutcome oppChoice myChoice = 
    match oppChoice, myChoice with
    | Rock, Paper
    | Paper, Scissors
    | Scissors, Rock -> WinScore
    | _, _ when oppChoice = myChoice -> DrawScore
    | _ -> LossScore

let getRoundScore (oppChoice, myChoice) = (getRoundOutcome oppChoice myChoice) + (getChoiceScore myChoice)

type Input = (string * string)

let parseFile : string list -> Input list =
    List.map (fun x -> 
        x.Split(' ')
        |> Array.take 2
        |> List.ofArray
        |> List.toTupleOf2)

let part1 (input : Input list) =
    input
    |> List.map (fun (x1, x2) -> (toOpponentChoice x1, toMyChoice x2))
    |> List.map getRoundScore
    |> List.sum


let getDesiredOutcome = function
    | Draw, x -> x
    | Loss, Rock -> Scissors
    | Loss, Paper -> Rock
    | Loss, Scissors -> Paper
    | Win, Rock-> Paper
    | Win, Paper-> Scissors
    | Win, Scissors-> Rock

let toOutcome = function
    | "X" -> Loss
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "invalid outcome"

let part2 (input : Input list) =
    input 
    |> List.map (fun (x1, x2) -> (toOpponentChoice x1, toOutcome x2))
    |> List.map (fun (oppChoice, outcome) -> (oppChoice, getDesiredOutcome (outcome, oppChoice)))
    |> List.map getRoundScore
    |> List.sum