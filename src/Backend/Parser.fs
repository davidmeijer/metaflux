module Backend.Parser

open System
open MathNet.Numerics.LinearAlgebra
open Backend.Types

let initPathway (stoichiometry: float[,]) (fluxNames: string[]) (metaboliteNames: string[]) =
    let stoichiometry = Matrix.Build.DenseOfArray stoichiometry
    if stoichiometry.ColumnCount = fluxNames.Length
       && stoichiometry.RowCount = metaboliteNames.Length
       then {
            Stoichiometry = stoichiometry
            FluxNames = fluxNames
            MetaboliteNames = metaboliteNames
       }
    else
        Exception "Number of flux and metabolite names do not match dimensionality stoichiometric matrix!"
        |> raise

let parseNetworkFile (path: string) =
    let lines = [
        use reader = new IO.StreamReader(path)
        while not reader.EndOfStream do yield reader.ReadLine()
    ]
    match lines with
    | header::rest ->
        let fluxNames = header.Split ',' |> Array.filter (fun s -> s.Length <> 0)
        let rec parse (data: list<string>) stoichiometry metabNames =
            match data with
            | line::otherLines ->
                let items = line.Split ','
                let newStoichiometry =
                    items.[1 .. items.Length - 1]
                    |> Array.map (fun s -> float s)
                    |> Array.toList
                parse otherLines (stoichiometry @ [ newStoichiometry ]) (metabNames @ [ items.[0] ])
            | _ ->
                array2D stoichiometry,
                metabNames |> List.toArray
        let stoichiometry, metaboliteNames = parse rest [] []
        initPathway stoichiometry fluxNames metaboliteNames
    | _ ->
        Exception "Could not parse input network file!"
        |> raise
        
        
        