module Backend.Types

open MathNet.Numerics.LinearAlgebra

type Pathway = {
    Stoichiometry : Matrix<float>
    FluxNames : array<string>
    MetaboliteNames : array<string>
}
    with
     /// Return the number of fluxes in the stoichiometric matrix.
    member this.NumFluxes () = this.Stoichiometry.ColumnCount
        
    /// Return the number of balances in the stoichiometric matrix.
    member this.NumBalances () = this.Stoichiometry.RowCount
        
    /// Return the number of independent balances in the stoichiometric matrix.
    member this.NumIndependentBalances () = this.Stoichiometry.Rank()
    
    /// Return then degrees of freedom for the metabolic network.
    member this.NumDegreesOfFreedom () = this.NumFluxes() - this.NumIndependentBalances()
    
    /// Return the number of conserved moieties present in the metabolic network.
    member this.NumConservedMoieties () = this.NumBalances() - this.NumIndependentBalances()