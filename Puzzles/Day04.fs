module AoC_2024.Day04

open System
module Helper =
    let containsXMAS (span: ReadOnlySpan<char>) : bool =
            (span.SequenceEqual ("XMAS")) || (span.SequenceEqual ("SAMX"))

    let containsXMASSpan (span: Span<char>) : bool =
            (span.SequenceEqual ("XMAS")) || (span.SequenceEqual ("SAMX"))

    let containsMAS (arr: char array) : bool =
        match arr with
        |[|'M'; 'A'; 'S'|] 
        |[|'S'; 'A'; 'M'|] -> true
        | _ -> false


let solve (input: string array) : (string * string) = 
    let mutable part1: int64 = 0
    let mutable part2: int64 = 0

    let lines_length = input[0].Length
    let input_length = input.Length

    let arr_fd = Array.create 4 '0'
    let arr_bd = Array.create 4 '0'
    let arr_ver = Array.create 4 '0'
    
    for i in 0..(input_length - 1) do
        for j in 0..(lines_length - 1) do
            if (j + 4 <= lines_length) && (i + 4 <= input_length) then
                for k in 0..3 do Array.set arr_fd k (input[i + k][j + k])   
                if Helper.containsXMASSpan (arr_fd.AsSpan ()) then 
                    part1 <- part1 + 1L
                    
            if (j >= 3) && (i + 4 <= input_length) then
                for k in 0..3 do Array.set arr_bd k (input[i + k][j - k])
                if Helper.containsXMASSpan ((arr_bd.AsSpan ())) then 
                    part1 <- part1 + 1L

            if (j + 4 <= lines_length) then
                let span_vertical = input[i].AsSpan (j, 4)
                if Helper.containsXMAS span_vertical then
                    part1 <- part1 + 1L
                
            if (i + 4 <= input_length) then
                for k in 0..3 do Array.set arr_ver k (input[i + k][j])
                if Helper.containsXMASSpan (arr_ver.AsSpan ()) then 
                    part1 <- part1 + 1L

    let arr1 = Array.create 3 '0'
    let arr2 = Array.create 3 '0'

    for i in 1..(input_length - 2) do
        for j in 1..(lines_length - 2) do 
            if (input[i][j] = 'A') then
                Array.set arr1 0 (input[i - 1][j - 1])
                Array.set arr1 1 (input[i][j])
                Array.set arr1 2 (input[i + 1][j + 1])

                Array.set arr2 0 (input[i + 1][j - 1])
                Array.set arr2 1 (input[i][j])
                Array.set arr2 2 (input[i - 1][j + 1])

                if (Helper.containsMAS arr1) && (Helper.containsMAS arr2) then
                    part2 <- part2 + 1L
                
    ($"{part1}", $"{part2}")