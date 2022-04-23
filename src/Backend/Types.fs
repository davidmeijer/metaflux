module Backend.Types

open MathNet.Numerics.LinearAlgebra

type Pathway = {
    Stoichiometry : Matrix<float>
    FluxNames : array<string>
    MetaboliteNames : array<string>
}
    with
    member this.PrintReactions () =
        this.FluxNames
        |> Array.zip [| 0 .. this.FluxNames.Length - 1 |]
        // Match coefficients with metabolite names for every flux.
        |> Array.map (fun (i, n) ->
            let coeffs =
                [|
                    for i in [ 0 .. this.Stoichiometry.RowCount - 1 ] do
                        yield (this.Stoichiometry.Row i).ToArray()
                |]
                |> Array.zip [| 0 .. this.MetaboliteNames.Length - 1 |]
                |> Array.map (fun (j, row) -> this.MetaboliteNames.[j], row.[i])
            n, coeffs)
        // Filter out all metabolites with coefficients that are zero.
        |> Array.map (fun (n, coeffs) ->
            n, coeffs |> Array.filter (fun (_, coeff) -> coeff <> 0.0))
        // Create string representation of all non-negative coefficients.
        |> Array.map (fun (n, coeffs) ->
            let coeffsToFormula coeffs =
                coeffs
                |> Array.map (fun (metab, coeff) -> $"{coeff} {metab}")
                |> String.concat " + "
            let positive =
                coeffs
                |> Array.filter (fun (_, coeff) -> coeff > 0.0)
                |> coeffsToFormula
            let negative =
                coeffs
                |> Array.filter (fun (_, coeff) -> coeff < 0.0)
                |> Array.map (fun (metab, coeff) -> metab, -1.0 * coeff)
                |> coeffsToFormula
            let noReactantsMsg = "outside"
            $"{n}: " +
            $"{if negative.Length <> 0 then negative else noReactantsMsg} " +
            "--> " +
            $"{if positive.Length <> 0 then positive else noReactantsMsg} ")
        |> String.concat "\n"
        |> printf "\nReactions\n---------\n%s\n"
            
    member this.PrintBalances () =
         this.MetaboliteNames
         |> Array.zip [| 0 .. this.MetaboliteNames.Length - 1 |]
         |> Array.map (fun (i, metab) ->
             let coeffs =
                 (this.Stoichiometry.Row i).ToArray()
                 |> Array.zip [| 0 .. this.FluxNames.Length - 1 |]
                 |> Array.map (fun (j, coeff) -> this.FluxNames.[j], coeff)
                 |> Array.filter (fun (_, coeff) -> coeff <> 0.0)
             let influxes =
                 coeffs
                 |> Array.filter (fun (_, coeff) -> coeff > 0.0)
                 |> Array.map (fun (fluxName, coeff) -> $"{coeff} * {fluxName}")
                 |> String.concat " + "
             let outfluxes =
                 coeffs
                 |> Array.filter (fun (_, coeff) -> coeff < 0.0)
                 |> Array.map (fun (fluxName, coeff) -> $"{-1.0 * coeff} * {fluxName}")
                 |> String.concat " + "
             $"Metabolite {metab}:\n\tInfluxes = {influxes}\n\tOutfluxes = {outfluxes}")
         |> String.concat "\n"
         |> printf "\nBalances\n--------\n%s\n"
         