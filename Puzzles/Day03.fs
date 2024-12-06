module AoC_2024.Day03

open System

module Helper =
    let spanCharToInt (span: ReadOnlySpan<char>) : (int64 * bool) = 
        let mutable acc: int64 = 0
        let mutable valid = true

        for i in 0..(span.Length - 1) do
            let char_is_valid = Char.IsNumber span[i]

            if not char_is_valid then 
                valid <- false
                acc <- 0
            elif valid then
                acc <- (acc * 10L) + (int64)(span[i] - '0')

        (acc, valid)

    let processRestOfSpan (span: ReadOnlySpan<char>) : int64 =
        let index_of_comma = span.IndexOf (',')
        let index_of_bracket = span.IndexOf (')')

        match (index_of_comma, index_of_bracket) with
        | (-1, _) -> 0
        | (_, -1) -> 0
        | (_, _) when index_of_comma > index_of_bracket -> 0
        | (num, _) when num = (span.Length - 1) -> 0
        | _ ->
            let (first_num, couldParseFirst) = spanCharToInt (span.Slice (0, index_of_comma))
            let (second_number, couldParseSecond) = spanCharToInt (span.Slice (index_of_comma + 1, index_of_bracket - index_of_comma - 1))

            match (couldParseFirst, couldParseSecond) with
            | (true, true) -> first_num * second_number
            | _ -> 0

let solve (input: string array) : (string * string) = 
    let mutable p1: int64 = 0

    for line in input do
        for i in 0..line.Length - 4 do

            let span = line.AsSpan (i, 4)

            if (span.SequenceEqual ("mul(")) then
                let start_of_num = i + 4
                let proc_length = 8

                if start_of_num + proc_length < line.Length then
                    p1 <- p1 + Helper.processRestOfSpan (line.AsSpan (start_of_num, 8))
                else 
                    p1 <- p1 + Helper.processRestOfSpan (line.AsSpan(start_of_num))

    let mutable p2: int64 = 0
    let mutable enabled = true 

    for line in input do
        for i in 0..line.Length - 4 do

            if (i + 7 <= line.Length) then
                let possible_dont = line.AsSpan (i, 7)

                if possible_dont.SequenceEqual ("don't()") then
                    enabled <- false

            let possible_do_and_mul = line.AsSpan (i, 4)
            
            if possible_do_and_mul.SequenceEqual("do()") then
                enabled <- true

            elif possible_do_and_mul.SequenceEqual("mul(") && enabled then
                let start_of_num = i + 4
                let proc_length = 8

                if start_of_num + proc_length < line.Length then
                    p2 <- p2 + Helper.processRestOfSpan (line.AsSpan (start_of_num, 8))
                else 
                    p2 <- p2 + Helper.processRestOfSpan (line.AsSpan(start_of_num))


    ($"{p1}", $"{p2}")