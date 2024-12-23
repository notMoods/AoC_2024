module Program

open System.IO
open AoC_2024

let input = 
    File.ReadAllLines "input.txt"


[<EntryPoint>]
let main _ =
    
    let (ans1, ans2) = Day24.solve input

    printfn "Part 1: %s Part 2: %s" ans1 ans2
    
    0