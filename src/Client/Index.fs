module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    Messages: Message list
    IsMessagePanelExpanded: bool
}

type Msg =
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
    }
    model, Cmd.OfAsync.perform todosApi.getMessages () RetrievedMessages

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | RetrievedMessages messages ->
        { model with Messages = messages }, Cmd.none
    | ToggleMessagePanel ->
        // TODO: resize top window
        { model with IsMessagePanelExpanded = not model.IsMessagePanelExpanded }, Cmd.none

open Feliz
open Feliz.Bulma

let private editor =
    Html.div []

let private messagePanel (isExpanded: bool) (messages: Message list) dispatch =
    Html.div [
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
                                    Html.text "Messages"
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
        prop.children [
            editor
            messagePanel model.IsMessagePanelExpanded model.Messages dispatch
        ]
    ]
