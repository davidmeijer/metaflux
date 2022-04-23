namespace Cli

module Utils =
    
    open Argu
    
    type CliResult<'TSuccess, 'TFail> =
        | Success of string
        | Fail of string
        
    type CmdArgs =
        | [<AltCommandLine("-network")>] Network of network : string
        with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Network _ -> "network file."
                
    let getExitCode result =
        match result with
        | Success msg ->
            printfn $"{msg}"
            0
        | Fail msg ->
            printfn $"{msg}"
            1
        
module Program =
    
    open System
    open Argu
    open Utils
    open Backend
    
    let [<EntryPoint>] main argv =
        let errorHandler = ProcessExiter(colorizer = function
            | ErrorCode.HelpText -> None
            | _ -> Some ConsoleColor.Red)
        
        let parser =
            ArgumentParser
                .Create<CmdArgs>(
                    programName = "metaflux",
                    errorHandler = errorHandler)
                         
        match parser.ParseCommandLine argv with
        | p ->
            match p.Contains(Network) with
            | true ->
                let networkFilePath = p.GetResult(Network)
                let pathway = Parser.parseNetworkFile networkFilePath
                pathway.PrintReactions()
                pathway.PrintBalances()
                CliResult.Success "Success!"
            | _ ->
                printfn $"{parser.PrintUsage()}"
                CliResult.Fail "Path to network file not specified!"
        |> getExitCode