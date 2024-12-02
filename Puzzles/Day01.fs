module AoC_2024.Day01

open System 
let solve (input: string array) : (string * string) = 

    let a_dict = Collections.Generic.Dictionary<int, int> ()
    
    let both_lists = [|
        for line in input do
            let arr = line.Split (" ", StringSplitOptions.RemoveEmptyEntries)

            let num1 = Int32.Parse arr.[0]
            let num2 = Int32.Parse arr.[1]

            if a_dict.ContainsKey num2 then
                a_dict.[num2] <- a_dict.[num2] + 1
            else
                a_dict.Add (num2, 1)

            (num1, num2)     
    |]

    let sorted_lists = 
        Array.zip (Array.sort [|
            for (x, _) in both_lists do x |])
            (Array.sort [| for (_, y) in both_lists do y |])

    let part1 = 
        sorted_lists
        |> Array.fold (fun (num: int) (x, y) -> num + (Math.Abs (x - y))) 0


    let part2 = 
        both_lists
        |> Array.fold (fun acc (first, _) -> 
                        if a_dict.ContainsKey first then
                            acc + (first * a_dict.[first])
                        else acc + 0 ) 0

    ($"{part1}", $"{part2}")