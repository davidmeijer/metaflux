module Backend.PathwayUtils

open MathNet.Numerics.LinearAlgebra
open Backend.GeneralUtils
open Backend.Types

/// List reactions from the pathway stoichiometric matrix.  
let printReactions (pathway: Pathway) =
    let title = "Reactions"
    let msg =
        pathway.FluxNames
        |> Array.zip [| 0 .. pathway.FluxNames.Length - 1 |]
        // Match coefficients with metabolite names for every flux.
        |> Array.map (fun (i, n) ->
            let coeffs =
                [|
                    for i in [ 0 .. pathway.Stoichiometry.RowCount - 1 ] do
                        yield (pathway.Stoichiometry.Row i).ToArray()
                |]
                |> Array.zip [| 0 .. pathway.MetaboliteNames.Length - 1 |]
                |> Array.map (fun (j, row) -> pathway.MetaboliteNames.[j], row.[i])
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
    printf $"\n{title}\n{underLine title}\n{msg}\n"

/// List the balances from the pathway stoichiometric matrix.
let printBalances (pathway: Pathway) =
     let title = "Balances"
     let msg =
         pathway.MetaboliteNames
         |> Array.zip [| 0 .. pathway.MetaboliteNames.Length - 1 |]
         |> Array.map (fun (i, metab) ->
             let coeffs =
                 (pathway.Stoichiometry.Row i).ToArray()
                 |> Array.zip [| 0 .. pathway.FluxNames.Length - 1 |]
                 |> Array.map (fun (j, coeff) -> pathway.FluxNames.[j], coeff)
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
     printf $"\n{title}\n{underLine title}\n{msg}\n"

/// List pathway structural network analyses.
let printStructuralNetWorkAnalysis (pathway: Pathway) =
    let title = "Structural Network Analysis"
   
    let numConservedMoieties = pathway.NumConservedMoieties()
    let nullSpace = pathway.Stoichiometry.Kernel() |> Matrix.Build.DenseOfColumnVectors
    
    // Redundant equations can be identified by analysis of the null space matrix of
    // the transposed stoichiometric matrix. If there are spanning vectors, there
    // are conserved moieties. Each vector represents a conserved moiety and at least
    // one of the non-zero entries can be removed.
    let conservedMoietiesMsg =
        if numConservedMoieties = 0 then
            "No conserved moieties present in metabolic network"
        else
            let conservedMetabolites =
                [ for i in [ 0 .. nullSpace.ColumnCount - 1 ] do
                    let nonZeroEntities =
                        (nullSpace.Column i).ToArray()
                        |> Array.map (fun v -> v <> 0.0)
                        |> Array.zip pathway.MetaboliteNames
                        |> Array.filter (fun (_, b) -> b)
                        |> Array.map (fun (metab, _) -> metab)
                    yield i, nonZeroEntities ]
            let msg =
                conservedMetabolites
                |> List.map (fun (i, metabs) ->
                    let metabsMsg = String.concat ", " metabs 
                    $"Vector {i}: {metabsMsg}")
                |> String.concat "\n\t"
            $"Found {numConservedMoieties} conserved moieties: {msg}"
    
    // A blocked reaction cannot have a value different to 0 -- thus it will not be
    // present in any solution-vector of the null space.
    let blockedReactionsMsg =
        let blocked =
            [ for i in [ 0 .. nullSpace.RowCount - 1 ] do
                if (nullSpace.Row i).ForAll (fun v -> v < 1e-12) then
                    yield pathway.FluxNames.[i] ]
        if blocked.Length = 0 then
            "No blocked reactions present in metabolic network"
        else
            let blockedItems = blocked |> String.concat ", "
            $"Blocked reactions: {blockedItems}"
            
    // Internal cycles are reactions that are decoupled from the extracellular observables (fluxes).
    // I.e., a set of reactions that can operate when all extracellular rates are 0. Therefore, the
    // null space for this scenario with all extracellular rates as 0 is calculated. Any solution found
    // is an internal cycle.
    let internalCyclesMsg =
        "Internal cycles check not yet implemented" // TODO
            
    let msg =
        [
            $"Number of fluxes: {pathway.NumFluxes()}"
            $"Number of balances: {pathway.NumBalances()}"
            $"Number of independent balances: {pathway.NumIndependentBalances()}"
            $"Degrees of freedom metabolic network: {pathway.NumDegreesOfFreedom()}"
            conservedMoietiesMsg
            blockedReactionsMsg
            internalCyclesMsg
        ]
        |> String.concat "\n"
    printf $"\n{title}\n{underLine title}\n{msg}\n"
     
   
