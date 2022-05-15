module Index

open System
open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    Messages: Message list
    IsMessagePanelExpanded: bool
    Nodes: Node list
}

type Msg =
    | AddNode
    | RemoveNode of Guid
    | SetNodeName of Guid * string
    | RetrievedMessages of Message list
    | ToggleMessagePanel

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
    model, Cmd.OfAsync.perform todosApi.getMessages () RetrievedMessages

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AddNode ->
        { model with Nodes = model.Nodes @ [ Node.create 0.0 0.0 "new node" ] }, Cmd.none
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

open Feliz
open Feliz.Bulma
open Fable.Core

open Browser
open Browser.Types
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Fulma



let private editor nodes dispatch =
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
                    prop.children [
                        Html.div [
                            prop.className "node-button-ribbon"
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
//                        Html.div [
//                            prop.className "node-name"
//                            prop.text node.Name
//                        ]
                        div [ Class "input-container" ] [
                            Field.div [  ] [
                                Control.p [ Control.IsExpanded ] [
                                    Input.input [
                                        Input.Value node.Name
                                        Input.Props [ DOMAttr.OnChange (fun ev -> (node.Id, ev.Value) |> SetNodeName |> dispatch)
                                                      HTMLAttr.SpellCheck false ]
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
