module AoC_2024.Day05

open System

module Helper =
    let isOrdered (arr: int array) (page_rules: Collections.Generic.HashSet<(int * int)>): bool =
        let mutable valid = true

        for i in 0..(arr.Length - 1) do
            for j in (i + 1)..(arr.Length - 1) do
                if page_rules.Contains ((arr[i], arr[j])) then
                    valid <- false

        valid

    let order (arr: int array) (page_rules: Collections.Generic.HashSet<(int * int)>) : int array =
        let res = Array.copy arr

        //bubble sort this joint
        for i in 0..(res.Length - 2) do
            for j in 0.. (res.Length - i - 2) do
                if (page_rules.Contains ((res[j], res[j + 1]))) then
                    let temp = res[j]
                    res[j] <- res[j + 1]
                    res[j + 1] <- temp

        res 

let solve (input: string array) : (string * string) =
    let index_of_space = Array.IndexOf (input, String.Empty)
        
    let mutable part1 = 0
    let mutable part2 = 0

    let page_rules_set = Collections.Generic.HashSet<(int * int)> (
                    [|
                        for string in input[..(index_of_space - 1)] do
                            let arr = string.Split ('|', StringSplitOptions.RemoveEmptyEntries)
                            (Int32.Parse arr[1], Int32.Parse arr[0])
                    |])

    let pages = 
        input[(index_of_space + 1)..]
        |> Array.map (fun x -> 
                        let arr = x.Split (',', StringSplitOptions.RemoveEmptyEntries)
                        [|for num in arr -> Int32.Parse num|])

    pages
    |> Array.iter (fun page ->
                    if Helper.isOrdered page page_rules_set then
                        part1 <- part1 + page[page.Length / 2]
                    else
                        let fixed_page = Helper.order page page_rules_set
                        part2 <- part2 + fixed_page[fixed_page.Length / 2])

    ($"{part1}", $"{part2}")