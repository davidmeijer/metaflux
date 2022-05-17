module Index

open Browser
open Browser.Types
open Elmish
open Fable.Remoting.Client
open Fable.React
open Fable.React.Props
open Feliz
open Fulma
open Shared
open System

type Position = { X: float; Y: float }

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
    Edges: Shared.Edge list
}

type Msg =
    | AddNode
    | RemoveNode of Guid
    | SetNodeName of Guid * string
    | RetrievedMessages of Message list
    | ToggleMessagePanel
    | MouseUp
    | MouseMove of Position
    | NodeDrag of Position
    | NodeDragStarted of Guid * Position
    | NodeDragEnded
    | ToggleEdgeDropdown of Guid
    | AddEdge of Guid * Guid

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = {
        Messages = []
        IsMessagePanelExpanded = true
        Nodes = []
        Edges = []
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
        let x = window.innerWidth / 2.0
        let y = window.innerHeight / 2.0
        let name = "new node"
        { model with Nodes = model.Nodes @ [ Shared.Node.create x y name ] }, Cmd.none
    | RemoveNode guid ->
        let nodes = model.Nodes |> List.filter (fun n -> n.Id <> guid)

        // Close open edge-dropdown windows if a single node is left.
        let nodes =
            if nodes.Length <= 1 then
                [ for node in nodes do yield { node with EdgesDropdownActivated = false } ]
            else
                nodes

        // Only keep edges for which source and target nodes still exist.
        let edges = [
            for edge in model.Edges do
                if edge.Source <> guid && edge.Target <> guid then
                    yield edge
        ]

        { model with Nodes = nodes; Edges = edges }, Cmd.none
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
    | MouseMove (position: Position) ->
        model, Cmd.ofMsg (NodeDrag position)
    | NodeDragStarted (guid, position) ->
        let elmnt = document.getElementById(string guid)
        let nodes = [
            for node in model.Nodes do
                if node.Id = guid then
                    yield {
                        node with
                            DragTarget = Dragging
                            OffsetX = position.X - elmnt.offsetLeft
                            OffsetY = position.Y - elmnt.offsetTop
                    }
                else
                    yield node
        ]
        { model with Nodes = nodes }, Cmd.none
    | NodeDragEnded ->
        let nodes = [
            for node in model.Nodes do
                yield {
                    node with
                        DragTarget = NoTarget
                        OffsetX = 0.0
                        OffsetY = 0.0
                }
        ]
        { model with Nodes = nodes }, Cmd.none
    | NodeDrag (position: Position) ->
        let nodes = [
            for node in model.Nodes do
                if node.DragTarget = DragTarget.Dragging then
                    yield {
                        node with
                            X = position.X - node.OffsetX
                            Y = position.Y - node.OffsetY
                    }
                else
                    yield node
        ]
        { model with Nodes = nodes }, Cmd.none
    | ToggleEdgeDropdown guid ->
        let nodes = [
            for node in model.Nodes do
                if node.Id = guid then
                    yield { node with EdgesDropdownActivated = not node.EdgesDropdownActivated }
                else
                    yield node
        ]
        { model with Nodes = nodes }, Cmd.none
    | AddEdge (source: Guid, target: Guid) ->
        // Only add directed edge if it does not yet exist.
        let edges =
            if [ for e in model.Edges do yield e.Source = source && e.Target = target ] |> List.contains true then
                model.Edges
            else
                model.Edges @ [ { Source = source; Target = target } ]
        { model with Edges = edges }, Cmd.none

let private editor (nodes: Shared.Node list) (edges: Shared.Edge list) dispatch =
    Html.div [
        prop.className "editor"
        prop.children [
            // Include "add node" button.
            Html.div [
                prop.className "button"
                prop.onClick (fun _ -> dispatch AddNode)
                prop.children [
                    Html.i [
                        prop.className "fas fa-plus"
                    ]
                ]
            ]

            // Include edges.
            for edge in edges do
                let getPos (guid: Guid) =
                    match nodes |> List.filter (fun n -> n.Id = guid) with
                    | first::rest when rest.Length = 0 ->
                        let elmnt = document.getElementById(string first.Id)
                        let offsetX, offsetY = elmnt.clientWidth / 2.0, elmnt.clientHeight / 2.0
                        { X = first.X + offsetX; Y = first.Y + offsetY }
                    | _ -> raise (Exception($"Edge connecting to node {guid} does not exist!"))
                let src, tgt = getPos edge.Source, getPos edge.Target
                let centerX = (src.X + tgt.X) / 2.0
                let centerY = (src.Y + tgt.Y) / 2.0
                let angle = (Math.Atan2(src.Y - tgt.Y, src.X - tgt.X) * 180.0) / Math.PI
                let dist = Math.Sqrt(Math.Pow((tgt.X - src.X), 2.0) + Math.Pow((tgt.Y - src.Y), 2.0))

                Html.div [
                    prop.className "edge"
                    prop.style [
                        style.width (length.px dist)
                        style.transform.rotate angle
                        style.top (length.px centerY)
                        style.left (length.px (centerX - (dist / 2.0)))
                    ]
                ]

            // Include nodes.
            for node in nodes do
                Html.div [
                    prop.id (string node.Id)
                    prop.className "node"
                    prop.style [
                        style.left (length.px node.X)
                        style.top (length.px node.Y)
                    ]
                    prop.children [
                        Html.div [
                            prop.className "node-button-ribbon"
                            prop.onMouseDown (fun ev ->
                                ev.preventDefault()
                                let coordsMouseDown = { X = ev.pageX; Y = ev.pageY }
                                dispatch (NodeDragStarted (node.Id, coordsMouseDown)))
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
                        div [ ] [
                            Field.div [ ] [
                                Control.p [ Control.IsExpanded ] [
                                    Input.input [
                                        Input.CustomClass "node-name-input-container"
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
                        let otherNodes = nodes |> List.filter (fun n -> n.Id <> node.Id)
                        let availableEdges = otherNodes.Length <> 0
                        Html.div [
                            prop.className "node-select-edges"
                            prop.children [
                                Dropdown.dropdown [
                                    Dropdown.CustomClass "node-select-edges-dropdown"
                                    Dropdown.IsActive node.EdgesDropdownActivated
                                ] [
                                    Dropdown.trigger [
                                        GenericOption.CustomClass "node-select-edges-trigger"
                                    ] [
                                        Button.button [
                                            Button.CustomClass "node-select-edges-button"
                                            Button.Option.IsFullWidth
                                            Button.Option.Disabled (not availableEdges)
                                            Button.OnClick (fun _ -> dispatch (ToggleEdgeDropdown node.Id))
                                        ] [
                                            span [  ] [ str "edge to..." ]
                                        ]
                                    ]
                                    Dropdown.menu [  ] [
                                        div [ Class "node-select-edges-dropdown-menu" ] [
                                            for target in otherNodes do
                                                Dropdown.Item.a [
                                                    Dropdown.Item.Option.CustomClass "node-select-edges-dropdown-menu-item"
                                                    Dropdown.Item.Props [
                                                        OnClick (fun _ -> dispatch (AddEdge (node.Id, target.Id)))
                                                    ]
                                                ] [
                                                    Html.i [
                                                        prop.className "fas fa-angle-right"
                                                    ]
                                                    str (" " + target.Name)
                                                ]
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
    let title = if numMessages = 0 then "Messages " else $"Messages ({numMessages}) "
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
                                    Html.i [
                                        prop.className ("fas " + (if isExpanded then "fa-angle-up" else "fa-angle-down"))
                                    ]
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
            editor model.Nodes model.Edges dispatch
        ]
    ]
