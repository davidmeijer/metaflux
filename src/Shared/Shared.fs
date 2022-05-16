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
    OffsetX: float
    OffsetY: float
    Extracellular: bool
    Name: string
    DragTarget: DragTarget
}

type Edge = {
    Source: Guid
    Target: Guid
}

module Message =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not
    let create (description: string) = {
        Id = Guid.NewGuid()
        Description = description
    }

module Node =
    let create (x: float) (y: float) (name: string) = {
        Id = Guid.NewGuid()
        X = x
        Y = y
        OffsetX = 0.0
        OffsetY = 0.0
        Extracellular = false
        Name = name
        DragTarget = NoTarget
    }

module Edge =
    let create (source: Guid) (target: Guid) = {
        Source = source
        Target = target
    }

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getMessages: unit -> Async<Message list>
}
