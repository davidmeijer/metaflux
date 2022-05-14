namespace Shared

open System

type Message = { Id: Guid; Description: string }

type Node = { Id: Guid; X: float; Y: float; Description: string }

module Message =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not
    let create (description: string) =
        { Id = Guid.NewGuid(); Description = description }

module Node =
    let create (x: float) (y: float) (description: string) =
        { Id = Guid.NewGuid(); X = x; Y = y; Description = description }

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getMessages: unit -> Async<Message list>
}
