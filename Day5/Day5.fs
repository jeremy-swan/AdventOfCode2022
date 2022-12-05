module Day5
open TypeExtensions
open System

let TEST_INPUT = "day5_test.txt"
let REAL_INPUT = "day5_real.txt"

type Instruction = {
    count: int
    fromStack: int
    toStack: int
}
type Stack = char list
type Input = Stack list * Instruction list

let parseFile (input : string list): Input = 
    let parseLine =
        (Seq.toList
         >> List.chunkBySize 4
         >> List.map (fun crate -> 
             match crate[1] with
             | x when x <> ' ' -> Some x
             | _ -> None))

    let getStacks (input : char option list list) : char list list = 
        input
        |> List.transpose
        |> List.map (List.choose id)

    let toInstruction (line : string) = 
        let split = line.Split(' ')
        { count = split[1] |> Int32.Parse
          fromStack = (split[3] |> Int32.Parse) - 1
          toStack = (split[5] |> Int32.Parse) - 1 }

    let (rawStacks, rawInstructions) = 
        input 
        |> List.splitBy (fun x -> x = "") 
        |> List.toTupleOf2

    let stacks = 
        rawStacks
        |> List.map parseLine
        |> List.removeLast
        |> getStacks

    let instructions = 
        rawInstructions
        |> List.map toInstruction

    (stacks, instructions)


let performProcedure pickupFunction ((stacks, instructions) : Input) =
    instructions
    |> List.fold (fun stacks instruction -> 
        stacks
        |> List.mapi (fun index (stack : Stack) -> 
            match index, stack with
            | i, stack when i = instruction.fromStack -> List.skip instruction.count stack
            | i, stack when i = instruction.toStack -> (List.take instruction.count (stacks[instruction.fromStack]) |> pickupFunction) @ stack
            | _ -> stack)) stacks

let part1 (input : Input ) = 
    input 
    |> performProcedure List.rev
    |> List.map (List.head >> string)
    |> Seq.ofList
    |> String.concat ""

let part2 (input : Input) = 
    input 
    |> performProcedure id
    |> List.map (List.head >> string)
    |> Seq.ofList
    |> String.concat ""