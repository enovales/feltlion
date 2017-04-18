module feltlion

open Conversation
open System
open System.Collections.Concurrent
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

type State = 
    {
        conversations: ConcurrentBag<Conversation>
        activeDialogues: ConcurrentDictionary<string, Dialogue>
    }

// global state of available conversations, and all dialogues that have been started.
let state = 
    { 
        State.conversations = new ConcurrentBag<Conversation>()
        activeDialogues = new ConcurrentDictionary<string, Dialogue>()
    }

let newDialogueHandler(name: string, channel: string, uid: string) = 
    match (state.conversations |> Seq.tryFind (fun c -> c.name = name)) with
    | Some(c) ->
        let d = 
            {
                Dialogue.c = c
                n = None
                s = DialogueState.Started
            }

        if state.activeDialogues.TryAdd(channel, d) then
            { SlackResponse.responseType = InChannel; text = "Started new dialogue [" + name + "]" }
        else
            { SlackResponse.responseType = InChannel; text = "Couldn't start new dialogue [" + name + "]" }

    | _ -> { SlackResponse.responseType = Ephemeral; text = "Couldn't find dialogue named [" + name + "]" }

let dialogueRequestHandler(r: SlackRequest): SlackResponse = 
    let cmdOpt = 
        r.Text |> Option.map (fun t -> t.Split([| ' ' |]) |> List.ofArray)

    match (cmdOpt, r.ChannelId, r.UserId) with
    | (Some([cmd; dialogueName]), Some(channel), Some(userId)) when cmd = "newDialogue" ->
        newDialogueHandler(dialogueName, channel, userId)
    | (Some(cmd :: tail), Some(channel), Some(userId)) when cmd = "run" ->
        let d = state.activeDialogues.Item(channel)
        let (newDialogue, result) = runDialogue(d, String.Join(" ", tail))
        state.activeDialogues.TryUpdate(channel, newDialogue, d) |> ignore
        { SlackResponse.responseType = InChannel; text = result }
    | _ -> { SlackResponse.responseType = Ephemeral; text = "Unrecognized command or something else failed" }

let dialogueHandler(ctx: HttpContext) = 
    (
        let resp = 
            SlackRequest.FromHttpContext ctx
            |> dialogueRequestHandler

        resp.ToString()
        |> OK
    ) ctx

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
        POST >=> path "/echo" >=> dialogueHandler >=> Writers.setMimeType "application/json"
    ]

//////////////////////////////
// Testing content
//////////////////////////////
let insertTestContent() = 
    let c = 
        {
            Conversation.Conversation.startNodes = 
                [
                    {
                        ConversationNode.text = "This is the start of the conversation."
                        speaker = ConversationSpeaker.Computer
                        next = 
                            [
                                {
                                    ConversationNode.text = "This is the first reply."
                                    speaker = ConversationSpeaker.User([])
                                    next =
                                        [
                                            {
                                                ConversationNode.text = "End of first tree"
                                                speaker = ConversationSpeaker.Computer
                                                next = []
                                            }
                                        ]
                                }
                                {
                                    ConversationNode.text = "This is the second reply."
                                    speaker = ConversationSpeaker.User([])
                                    next = 
                                        [
                                            {
                                                ConversationNode.text = "End of second tree"
                                                speaker = ConversationSpeaker.Computer
                                                next = []
                                            }
                                        ]
                                }
                            ]
                    }
                ]
            name = "testconvo"
        }
    state.conversations.Add(c)
    ()

[<EntryPoint>]
let main argv =
    insertTestContent()
    startWebServer defaultConfig app
    0
    