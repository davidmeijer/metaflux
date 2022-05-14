namespace Shared

open System

type Message = { Id: Guid; Description: string }

module Message =
    let isValid (description: string) = String.IsNullOrWhiteSpace description |> not
    let create (description: string) = { Id = Guid.NewGuid(); Description = description }

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getMessages: unit -> Async<Message list>
}
