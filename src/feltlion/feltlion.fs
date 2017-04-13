module feltlion

open System
open System.Text
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

type SlackRequest =
    {
        Token       : string
        TeamId      : string
        TeamDomain  : string
        ChannelId   : string
        ChannelName : string
        UserId      : string
        UserName    : string
        Command     : string
        Text        : string
        ResponseUrl : string
    }
    static member FromHttpContext (ctx : HttpContext) =
        let get key =
            match ctx.request.formData key with
            | Choice1Of2 x  -> x
            | _             -> ""
        {
            Token       = get "token"
            TeamId      = get "team_id"
            TeamDomain  = get "team_domain"
            ChannelId   = get "channel_id"
            ChannelName = get "channel_name"
            UserId      = get "user_id"
            UserName    = get "user_name"
            Command     = get "command"
            Text        = get "text"
            ResponseUrl = get "response_url"
        }

let echoHandler(ctx: HttpContext) = 
    (
        SlackRequest.FromHttpContext ctx
        |> fun req ->
            "{ \"response_type\": \"in_channel\", \"text\": \"" + req.Text + "\"}"
            |> OK
    ) ctx

let app = 
    choose [
        POST >=> path "/echo" >=> echoHandler >=> Writers.setMimeType "application/json"
    ]

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0
    