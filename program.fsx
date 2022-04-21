type Matrix = array<array<int>>

let size (matrix: Matrix) = matrix.Length, matrix.[0].Length
let rank (matrix: Matrix) = 0 // TODO

type Pathway = {
    StoichiometricMatrix: Matrix
    FluxNames: array<string>
    MetaboliteNames: array<string>
} with
    /// Print flux reactions.
    member this.PrintReactions () =
        this.FluxNames
        |> Array.zip [|0 .. this.FluxNames.Length - 1|]
        // Match coefficients with metabolite names for every flux.
        |> Array.map (fun (i, n) ->
            let coeffs =
                this.StoichiometricMatrix
                |> Array.zip [|0 .. this.MetaboliteNames.Length - 1|]
                |> Array.map (fun (j, row) -> 
                    this.MetaboliteNames.[j], row.[i])
            n, coeffs
        )
        // Filter out all metabolites with coefficients as zero. 
        |> Array.map (fun (n, coeffs) -> 
            n, coeffs |> Array.filter (fun (metab, coeff) -> coeff <> 0)
        )
        // Create string representation of all non negative coefficients.
        |> Array.map (fun (n, coeffs) ->
            let coeffsToFormula coeffs = 
                coeffs 
                |> Array.map (fun (metab, coeff) -> $"{coeff} {metab}")
                |> String.concat (" + ")
            let positive = 
                coeffs 
                |> Array.filter (fun (metab, coeff) -> coeff > 0)
            let negative = 
                coeffs 
                |> Array.filter (fun (metab, coeff) -> coeff < 0)
                // Display negative coefficients without a minus sign. 
                |> Array.map (fun (metab, coeff) -> metab, -1 * coeff)
            $"{n}: {coeffsToFormula negative} --> {coeffsToFormula positive}"
        )
        |> String.concat ("\n")
        |> printf "\nReactions\n---------\n%s\n"

    /// Print metabolite balances.
    member this.PrintBalances () =
        this.MetaboliteNames
        |> Array.zip [|0 .. this.MetaboliteNames.Length - 1|]
        |> Array.map (fun (i, metab) -> 
            let coeffs = 
                this.StoichiometricMatrix.[i]
                |> Array.zip [|0 .. this.FluxNames.Length - 1|]
                |> Array.map (fun (j, coeff) -> this.FluxNames.[j], coeff)
                |> Array.filter (fun (_, coeff) -> coeff <> 0)
            let influxes = 
                coeffs
                |> Array.filter (fun (_, coeff) -> coeff > 0)
                |> Array.map (fun (fluxName, coeff) -> $"{coeff} * {fluxName}")
                |> String.concat (" + ")
            let outfluxes = 
                coeffs
                |> Array.filter (fun (_, coeff) -> coeff < 0)
                |> Array.map (fun (fluxName, coeff) -> $"{-1 * coeff} * {fluxName}")
                |> String.concat (" + ")
            $"Metabolite {metab}:\n\tInfluxes = {influxes}\n\tOutfluxes = {outfluxes}"
        )
        |> String.concat ("\n")
        |> printf "\nBalances\n--------\n%s\n"

    member this.NumBalances () = (size this.StoichiometricMatrix) |> fst
    member this.NumFluxes () = (size this.StoichiometricMatrix) |> snd 
    member this.NumIndependentBalances () = rank this.StoichiometricMatrix
    member this.DegreesOfFreedom () = 
        this.NumFluxes() - (rank this.StoichiometricMatrix)

let main =
    let pathway = {
        StoichiometricMatrix = [|
            [|  1; -1;  0;  0;  0;  0;  0;  0;  0|]
            [|  0;  2; -1;  0;  0; -1;  0;  0; -1|]
            [|  0;  0;  1; -1;  0;  0;  0;  0;  0|]
            [|  0;  0;  0;  1; -1;  0;  0;  0;  0|]
            [|  0;  0; -1;  0;  1;  1; -1;  0;  0|]
            [|  0;  0;  1;  1;  1; -1;  0; -1;  1|]
        |];
        FluxNames = [|
            "V1"; "V2"; "V3"; "V4"; "V5"; "V6"; "V7"; "V8"; "V9"
        |];
        MetaboliteNames = [|
            "G6P"
            "Pyr"
            "Cit"
            "aKG"
            "Mal"
            "CO2"
        |]
    }
    pathway.PrintReactions()
    pathway.PrintBalances()
    printf $"\nNumber of fluxes: {pathway.NumFluxes()}"
    printf $"\nNumber of balances: {pathway.NumBalances()}"
    printf $"\nNumber of independent balances: {pathway.NumIndependentBalances()}"
    printf $"\nMetabolic network has {pathway.DegreesOfFreedom()} degrees of freedom"
    0

[<EntryPoint>]
main |> printf "\n%A"