// Only intracellular pools are balanced. Extracellular metabolites 
// like Glc_ec, Mal_ec, EtOH_ec, and CO2_ec are only consumed or 
// produced -- they cannot be balanced.
let metabolite_stoichiometry = [
    [  1; -1;  0;  0;  0;  0;  0;  0;  0]
    [  0;  2; -1;  0;  0; -1;  0;  0; -1]
    [  0;  0;  1; -1;  0;  0;  0;  0;  0]
    [  0;  0;  0;  1; -1;  0;  0;  0;  0]
    [  0;  0; -1;  0;  1;  1; -1;  0;  0]
    [  0;  0;  1;  1;  1; -1;  0; -1;  1]
]

let metabolite_names = [
    "G6P"
    "Pyr"
    "Cit"
    "aKG"
    "Mal"
    "CO2"
]
 
let vBM = [
    -0.0500;
    -0.0833;
    -0.0250;
    -0.0500;
    -0.0625;
     0.2000;
]

let Me = [
     0.0000;
     0.0000;
     0.0000;
     0.0000;
     0.0000;
     1.0000;
]

let main =
    0

[<EntryPoint>]
main |> printf "%A"