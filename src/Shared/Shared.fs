namespace Shared

open System

type Message = { Id: Guid; Description: string }

type Node = { Id: Guid; X: float; Y: float; Extracellular: bool; Name: string }

module Message =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not
    let create (description: string) =
        { Id = Guid.NewGuid(); Description = description }

module Node =
    let create (x: float) (y: float) (name: string) =
        { Id = Guid.NewGuid(); X = x; Y = y; Extracellular = false; Name = name }

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getMessages: unit -> Async<Message list>
}
