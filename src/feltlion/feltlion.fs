module feltlion

open System
open System.Text
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

type SlackRequest =
    {
        Token       : string option
        TeamId      : string option
        TeamDomain  : string option
        ChannelId   : string option
        ChannelName : string option
        UserId      : string option
        UserName    : string option
        Command     : string option
        Text        : string option
        ResponseUrl : string option
    }
    static member FromHttpContext (ctx : HttpContext) =
        let get key =
            match ctx.request.formData key with
            | Choice1Of2 x  -> Some(x)
            | _             -> None
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

type SlackResponseType = 
    | Ephemeral
    | InChannel
    with
        override this.ToString() = 
            match this with
            | Ephemeral -> "ephemeral"
            | InChannel -> "in_channel"

type SlackResponse = 
    {
        responseType: SlackResponseType
        text: string
    }
    with
        override this.ToString() = 
            "{ \"response_type\": \"" + this.responseType.ToString() + "\", \"text\": \"" + this.text + "\"}"

let echoHandler(ctx: HttpContext) = 
    (
        SlackRequest.FromHttpContext ctx
        |> fun req ->
            let resp = 
                match req.Text with
                | Some(t) when not(String.IsNullOrEmpty(t)) -> { SlackResponse.responseType = InChannel; text = t }
                | _ -> { SlackResponse.responseType = Ephemeral; text = "You didn't specify anything to echo." }

            resp.ToString()
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
    