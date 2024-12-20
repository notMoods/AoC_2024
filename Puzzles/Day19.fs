module AoC_2024.Day19

open System
open System.Collections.Generic

module Helper = 
    let isPossible (display: string) (towel_patterns: HashSet<string>) =
        let n = display.Length

        let dp = Array.create (n + 1) false
        dp[0] <- true

        for i in 1..n do
            let mutable loop_break = false

            for j in 0..(i - 1) do

                if not loop_break then
                    let substr = display.Substring (j, i - j)
                    if dp[j] && towel_patterns.Contains substr then
                        dp[i] <- true
                        loop_break <- true

        dp[n]

    let differentWays (display: ReadOnlySpan<char>) (towel_patterns: HashSet<string>) =

        let dp = Array.create (display.Length + 1) 0L
        dp[display.Length] <- 1


        for i in (display.Length - 1)..(-1)..0 do
            for pattern in towel_patterns do
                let slice = display.Slice(i)
                if (MemoryExtensions.StartsWith (slice, pattern)) then
                    
                    dp[i] <- dp[i] + dp[pattern.Length + i]
        dp[0]

let solve (input: string array) : (string * string) = 
    let towel_patterns = HashSet<string>()

    input[0]
    |> (fun patterns ->
            let arr = patterns.Split (',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
            arr)
    |> Array.iter (
        fun str -> 
            towel_patterns.Add str |> ignore
    )

    let display_designs = 
        input[2..]

    let part_1 = 
        display_designs
        |> Array.fold (
            fun acc str ->
                if (Helper.isPossible str towel_patterns) then
                    acc + 1
                else acc
        ) 0

    let part_2 = 
        display_designs
        |> Array.fold (
            fun acc str ->
                acc + (Helper.differentWays (str.AsSpan ()) towel_patterns)
        ) 0L
    
    ($"{part_1}", $"{part_2}")