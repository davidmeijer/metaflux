module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type Storage() =
    let messages = ResizeArray<_>()
    member _.GetMessages() = List.ofSeq messages
    member _.AddMessage(message: Message) =
        if Message.isValid message.Description then
            messages.Add message
            Ok()
        else
            Error "Invalid message!"

let storage = Storage()

storage.AddMessage(Message.create "Welcome to MetaFlux!")
|> ignore

let todosApi = {
   getMessages = fun () -> async { return storage.GetMessages() }
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
