module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    Messages: Message list
    IsMessagePanelExpanded: bool
    Nodes: Node []
}

type Msg =
    | AddNode
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
        Nodes = [||]
    }
    model, Cmd.OfAsync.perform todosApi.getMessages () RetrievedMessages

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | AddNode ->
        // TODO: add draggable div node
        printf "AddNode not yet implemented"
        model, Cmd.none
    | RetrievedMessages messages ->
        { model with Messages = messages }, Cmd.none
    | ToggleMessagePanel ->
        { model with IsMessagePanelExpanded = not model.IsMessagePanelExpanded }, Cmd.none

open Feliz
open Feliz.Bulma

let private editor dispatch =
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
            editor dispatch
        ]
    ]
