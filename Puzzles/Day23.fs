module AoC_2024.Day23

open System
open System.Collections.Generic

module Helper = 
    type Polygon = {
        Comps: string array
    }
    with member this.Length () = this.Comps.Length

    let addConnection comp1 comp2 (network: Dictionary<string, HashSet<string>>) = 
        if network.ContainsKey comp1 then
            network.[comp1].Add comp2 |> ignore
        else 
            let hs = HashSet<string> ()
            hs.Add comp2 |> ignore
            network.Add (comp1, hs) 

        if network.ContainsKey comp2 then
            network.[comp2].Add comp1 |> ignore
        else 
            let hs = HashSet<string> ()
            hs.Add comp1 |> ignore
            network.Add (comp2, hs)

let solve (input: string array) : (string * string) = 
    let networks = Dictionary<string, HashSet<string>> ()
    let shapes = HashSet<Helper.Polygon>()

    input
    |> Array.iter (
        fun connection ->
            let comps = connection.Split ('-', StringSplitOptions.RemoveEmptyEntries)

            Helper.addConnection comps[0] comps[1] networks

            let mutual_comps = Linq.Enumerable.Intersect(networks[comps[0]], networks[comps[1]]) |> Seq.toArray

            for comp in mutual_comps do
                shapes.Add { Comps = [|comps[0]; comp; comps[1]|] } |> ignore

            let fixed_mutual_comps = HashSet<string> (mutual_comps)

            for i in 0..(mutual_comps.Length - 1) do
                for j in (i + 1)..(mutual_comps.Length - 1) do
                    if not (networks[mutual_comps[i]].Contains mutual_comps[j]) then
                        fixed_mutual_comps.Remove (mutual_comps[i]) |> ignore

            if Linq.Enumerable.Count(mutual_comps) > 0 then
                let arr = 
                    [|
                        yield comps[0]
                        yield! fixed_mutual_comps
                        yield comps[1]
                    |]

                shapes.Add { Comps = arr } |> ignore
                )

    let part_1 = 
        shapes
        |> Seq.filter 
            (fun polygon -> 
                if polygon.Length () <> 3 then
                    false
                elif polygon.Comps[0].StartsWith('t')
                    || polygon.Comps[1].StartsWith('t')
                    || polygon.Comps[2].StartsWith('t') then
                    true
                else false)
        |> Seq.length

    let part_2 =
        shapes
        |> Seq.maxBy (fun shape -> shape.Length())
        |> (fun polygon -> polygon.Comps)
        |> Array.sort
        |> (fun arr ->
                String.Join(",", arr))


    ($"{part_1}", $"{part_2}")