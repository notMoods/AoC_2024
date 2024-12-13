module AoC_2024.Day13

open System

module Helper = 
    type MachineBehavior = {
        A_X : Int64
        A_Y : Int64
        B_X : Int64
        B_Y : Int64

        Prize_X : Int64
        Prize_Y : Int64
    }

    let getMachineBehavior (machine: string array) = 
        let mutable index_of_a_x = (machine[0].IndexOf 'X') + 2
        let mutable index_of_a_y = (machine[0].IndexOf 'Y') + 2

        let mutable (A_X, A_Y) = (0, 0)
        while Char.IsDigit (machine[0][index_of_a_x]) do
            A_X <- (A_X * 10) + int (machine[0][index_of_a_x] - '0')
            index_of_a_x <- index_of_a_x + 1

        while index_of_a_y <= (machine[0].Length - 1) do
            A_Y <- (A_Y * 10) + int (machine[0][index_of_a_y] - '0')
            index_of_a_y <- index_of_a_y + 1


        let mutable index_of_b_x = (machine[1].IndexOf 'X') + 2
        let mutable index_of_b_y = (machine[1].IndexOf 'Y') + 2

        let mutable (B_X, B_Y) = (0, 0)
        while Char.IsDigit (machine[1][index_of_b_x]) do
            B_X <- (B_X * 10) + int (machine[1][index_of_b_x] - '0')
            index_of_b_x <- index_of_b_x + 1

        while index_of_b_y <= (machine[1].Length - 1) do
            B_Y <- (B_Y * 10) + int (machine[1][index_of_b_y] - '0')
            index_of_b_y <- index_of_b_y + 1

        let mutable index_of_p_x = (machine[2].IndexOf 'X') + 2
        let mutable index_of_p_y = (machine[2].IndexOf 'Y') + 2

        let mutable (P_X, P_Y) = (0, 0)
        while Char.IsDigit (machine[2][index_of_p_x]) do
            P_X <- (P_X * 10) + int (machine[2][index_of_p_x] - '0')
            index_of_p_x <- index_of_p_x + 1

        while index_of_p_y <= (machine[2].Length - 1) do
            P_Y <- (P_Y * 10) + int (machine[2][index_of_p_y] - '0')
            index_of_p_y <- index_of_p_y + 1

        {
            A_X = A_X
            A_Y = A_Y
            B_X = B_X
            B_Y = B_Y

            Prize_X = P_X
            Prize_Y = P_Y
        }

    let solveForAPressAndBPress (m: MachineBehavior) : (float * float) =
        let m_numerator = (m.A_Y * m.Prize_X) - (m.Prize_Y * m.A_X)
        let m_denominator = (m.A_Y * m.B_X) - (m.A_X * m.B_Y)

        let B_Press = float m_numerator / float m_denominator 

        let n_numerator = float m.Prize_X - (float m.B_X * B_Press)
        let n_denominator = float m.A_X

        let A_Press = n_numerator / n_denominator

        (A_Press, B_Press)

    let areWhole (num1: float, num2: float) : bool = Double.IsInteger num1 && Double.IsInteger num2

let solve (input: string array) : (string * string) =
    let machine_solver arr = 
        arr
        |> Array.map (fun mach ->
                        Helper.solveForAPressAndBPress mach)
        |> Array.filter (fun res -> Helper.areWhole res)
        |> Array.sumBy (fun (a_press, b_press) ->
                            (a_press * 3.0) + b_press)

    let machines_behavior = 
        input
        |> Array.filter (fun str -> str <> String.Empty)
        |> Array.chunkBySize 3
        |> Array.map ( fun machine -> Helper.getMachineBehavior machine)

    let part_1 = 
        machines_behavior
        |> machine_solver

    let part_2 = 
        machines_behavior
        |> Array.map (fun mach ->
                        { mach with 
                            Prize_X = 10000000000000L + mach.Prize_X
                            Prize_Y = 10000000000000L + mach.Prize_Y})
        |> machine_solver

    ($"{part_1}", $"{part_2}")