module Index

open Browser
open Browser.Types
open Elmish
open Fable.Core
open Fable.Remoting.Client
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Feliz
open Feliz.Bulma
open Fulma
open Shared
open System

type MousePosition = { X: float; Y: float }

[<RequireQualifiedAccess>]
module Cmd =
    let ups messageCtor =
        let handler dispatch = window.addEventListener("mouseup", fun _ -> dispatch messageCtor)
        [ handler ]

    let move messageCtor =
        let handler dispatch =
            window.addEventListener("mousemove", fun ev ->
                let ev = ev :?> MouseEvent
                { X = ev.pageX; Y = ev.pageY } |> messageCtor |> dispatch)
        [ handler ]

type Model = {
    Messages: Message list
    IsMessagePanelExpanded: bool
    Nodes: Shared.Node list
}

type Msg =
    | AddNode
    | RemoveNode of Guid
    | SetNodeName of Guid * string
    | RetrievedMessages of Message list
    | ToggleMessagePanel
    | MouseUp
    | MouseMove of MousePosition
    | NodeDrag of MousePosition
    | NodeDragStarted of Guid
    | NodeDragEnded

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = {
        Messages = []
        IsMessagePanelExpanded = true
        Nodes = []
    }
    let cmd = Cmd.batch [
        Cmd.OfAsync.perform todosApi.getMessages () RetrievedMessages
        Cmd.ups MouseUp
        Cmd.move MouseMove
    ]
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AddNode ->
        { model with Nodes = model.Nodes @ [ Shared.Node.create 0.0 0.0 "new node" ] }, Cmd.none
    | RemoveNode guid ->
        { model with Nodes = model.Nodes |> List.filter (fun n -> n.Id <> guid) }, Cmd.none
    | SetNodeName (guid, newName) ->
        let nodes = [
            for node in model.Nodes do
                if node.Id = guid then
                    yield { node with Name = newName }
                else
                    yield node
        ]
        { model with Nodes = nodes }, Cmd.none
    | RetrievedMessages messages ->
        { model with Messages = messages }, Cmd.none
    | ToggleMessagePanel ->
        { model with IsMessagePanelExpanded = not model.IsMessagePanelExpanded }, Cmd.none
    | MouseUp ->
        model, Cmd.ofMsg NodeDragEnded
    | MouseMove (position: MousePosition) ->
        model, Cmd.ofMsg (NodeDrag position)
    | NodeDragStarted guid ->
        let nodes = [
            for node in model.Nodes do
                if node.Id = guid then
                    yield { node with DragTarget = Dragging }
                else
                    yield node
        ]
        { model with Nodes = nodes }, Cmd.none
    | NodeDragEnded ->
        let nodes = [
            for node in model.Nodes do
                yield { node with DragTarget = NoTarget }
        ]
        { model with Nodes = nodes }, Cmd.none
    | NodeDrag (position: MousePosition) ->
        let nodes = [
            for node in model.Nodes do
                if node.DragTarget = DragTarget.Dragging then
                    yield { node with X = position.X; Y = position.Y }
                else
                    yield node
        ]
        {model with Nodes = nodes }, Cmd.none

let private editor (nodes: Shared.Node list) dispatch =
    Html.div [
        prop.className "editor"
        prop.children [
            Html.div [
                prop.className "button"
                prop.onClick (fun _ -> dispatch AddNode)
                prop.children [
                    Html.i [
                        prop.className "fas fa-plus"
                    ]
                ]
            ]
            for node in nodes do
                Html.div [
                    prop.className "node"
                    prop.style [
                        style.top (length.px node.Y)
                        style.left (length.px node.X)
                    ]
                    prop.children [
                        Html.div [
                            prop.className "node-button-ribbon"
                            prop.onMouseDown (fun ev ->
                                ev.preventDefault()
                                dispatch (NodeDragStarted node.Id))
                            prop.children [
                                Html.div [
                                    prop.className "node-button"
                                    prop.onClick (fun _ -> dispatch (RemoveNode node.Id))
                                    prop.children [
                                        Html.i [
                                            prop.className "fas fa-minus fa-xs"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                        div [ Class "input-container" ] [
                            Field.div [  ] [
                                Control.p [ Control.IsExpanded ] [
                                    Input.input [
                                        Input.Value node.Name
                                        Input.Props [
                                            DOMAttr.OnChange (fun ev ->
                                                dispatch (SetNodeName (node.Id, ev.Value)))
                                            HTMLAttr.SpellCheck false
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
        ]
    ]


let private messagePanel (isExpanded: bool) (messages: Message list) dispatch =
    let numMessages = messages.Length
    let title = if numMessages = 0 then "Messages" else $"Messages ({numMessages})"
    Html.div [
        prop.className "message-panel"
        prop.children [
            Html.div [
                prop.className "message-panel-header"
                prop.onClick (fun _ -> dispatch ToggleMessagePanel)
                prop.children [
                    Html.div [
                        prop.className "message-panel-header-title"
                        prop.children [
                            Html.span [
                                prop.children [
                                    Html.text title
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            Html.div [
                prop.className ("message-panel-body " + (if isExpanded then "" else "is-hidden"))
                prop.children [
                    for msg in messages do
                        Html.div [
                            prop.className "message-panel-message"
                            prop.children [
                                Html.span [
                                    prop.children [
                                        Html.text msg.Description
                                    ]
                                ]
                            ]
                        ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className "metaflux"
        prop.children [
            messagePanel model.IsMessagePanelExpanded model.Messages dispatch
            editor model.Nodes dispatch
        ]
    ]
