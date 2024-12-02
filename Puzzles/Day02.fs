module AoC_2024.Day02

open System

let solve (input: string array) : (string * string) =
    let is_safe (levels: int list) : bool = 
        let rec is_safe' (num: int) (levels: int list) (incr: bool) : bool =
            match levels with
            | [] -> true
            | head :: tail when (head - num < 4) && (head - num > 0) -> if incr then is_safe' head tail true else false
            | head :: tail -> if not incr && (num - head < 4) && (num - head > 0) then is_safe' head tail false else false

        let (head, tail) = (List.head levels, List.tail levels)
        is_safe' head tail (head < (List.head tail))

    let remove_one (levels: int list) : bool =
        let list_of_one_offs = 
            [| for i in 0..(levels.Length - 1) do 
                yield [ for j in 0..(levels.Length - 1) do if j <> i then yield levels[j] ]
                |]

        list_of_one_offs
        |> Array.exists (fun x -> is_safe x)

    let list_of_reports = 
        input
        |> Array.map (fun x -> x.Split (" ", StringSplitOptions.RemoveEmptyEntries))
        |> Array.map (fun arr ->
                        [for x in arr do int x] )
                           
    let mutable safe_count = 0
    let mutable extras = 0

    for levels in list_of_reports do
        if is_safe levels then
            safe_count <- safe_count + 1
        else 
            if remove_one levels then
                extras <- extras + 1

    ($"{safe_count}", $"{extras + safe_count}")