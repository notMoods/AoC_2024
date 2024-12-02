module Program

open System.IO
open AoC_2024

//look into a better way to get a path
let input = 
    File.ReadAllLines "input.txt"


[<EntryPoint>]
let main _ =
    
    let (ans1, ans2) = Day03.solve input

    printfn "Part 1: %s Part 2: %s" ans1 ans2
    
    0