namespace Shared

open System

type Message = { Id: Guid; Description: string }

type DragTarget =
    | NoTarget
    | Dragging

type Node = {
    Id: Guid
    X: float
    Y: float
    Extracellular: bool
    Name: string
    DragTarget: DragTarget
}

module Message =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not
    let create (description: string) =
        { Id = Guid.NewGuid(); Description = description }

module Node =
    let create (x: float) (y: float) (name: string) =
        { Id = Guid.NewGuid()
          X = 0.0
          Y = 0.0
          Extracellular = false
          Name = name
          DragTarget = NoTarget }

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getMessages: unit -> Async<Message list>
}
