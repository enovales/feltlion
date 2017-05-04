module feltlion

open Argu
open Conversation
open FSharp.Data
open FSharp.Data.JsonExtensions
open System
open System.Collections.Concurrent
open System.Drawing
open System.Text
open Suave
open Suave.Filters
open Suave.Logging
open Suave.Operators
open Suave.Successful

let log = Suave.Logging.Log.create("foo")

let logSimple(s: string) =
    Message.event Info s |> log.logSimple

type Arguments = 
    | [<Mandatory>] IncomingWebhookUrl of url: string
with
    interface IArgParserTemplate with
        member this.Usage = 
            match this with
            | IncomingWebhookUrl _ -> "incoming webhook URL to use"

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

type SlackButtonRequest = 
    {
        ChannelId: string option
        UserId: string option
        ActionName: string option
        ActionValue: string option
    }
    override this.ToString() = 
        String.Format("""{{ ChannelId = {0}, UserId = {1}, ActionName = {2}, ActionValue = {3} }}""", this.ChannelId, this.UserId, this.ActionName, this.ActionValue)

    static member FromHttpContext (ctx: HttpContext) = 
        let get key = 
            match ctx.request.formData key with
            | Choice1Of2 x -> Some(x)
            | _ -> None

        logSimple "SlackButtonRequest"
        logSimple ((ctx.request.form |> Map.ofList).ToString())

        let extractUser pl =
            let j = JsonValue.Parse(pl)
            j.TryGetProperty("user") |> Option.bind(fun urec -> urec.TryGetProperty("id")) |> Option.map(fun idOpt -> idOpt.AsString())
        let extractChannel pl =
            let j = JsonValue.Parse(pl)
            j.TryGetProperty("channel") |> Option.bind(fun crec -> crec.TryGetProperty("id")) |> Option.map(fun idOpt -> idOpt.AsString())
        let extractActionNameAndValue pl = 
            let j = JsonValue.Parse(pl)
            let firstAction = 
                j.TryGetProperty("actions") 
                |> Option.bind(fun aa -> aa.AsArray() |> Array.tryHead) 

            let actionNameOpt = 
                firstAction
                |> Option.bind(fun a -> a.TryGetProperty("name"))
                |> Option.map (fun n -> n.AsString())

            let actionValueOpt = 
                firstAction
                |> Option.bind(fun fo -> fo.TryGetProperty("value"))
                |> Option.map(fun v -> v.AsString())

            (actionNameOpt, actionValueOpt)

        let (user, channel, (actionName, actionValue)) = 
            match get "payload" with
            | Some(pl) -> (extractUser pl, extractChannel pl, extractActionNameAndValue pl)
            | _ -> (None, None, (None, None))

        {
            ChannelId = channel
            UserId = user
            ActionValue = actionValue
            ActionName = actionName
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

type SlackInteractiveAction = 
    {
        name: string
        text: string
        value: string
    }
    with
        override this.ToString() = 
            String.Format("""{{ "name": "{0}", "text": "{1}", "type": "button", "value": {2} }}""", this.name, this.text, this.value)

type SlackInteractiveResponse = 
    {
        text: string
        buttonPrompt: string
        fallback: string
        callbackId: string
        color: string
        actions: SlackInteractiveAction array
    }
    with
        override this.ToString() = 
            logSimple "SlackInteractiveResponse.ToString(): before attachmentsStrings"
            let attachmentsStrings = 
                this.actions 
                |> Array.map(fun a -> a.ToString())

            logSimple "SlackInteractiveResponse.ToString(): before attachments"
            let attachmentFormatString = 
                """{{ "text": "", "attachment_type": "default", "callback_id": "0", "actions": [{0}] }}"""
            let attachments = 
                String.Format(attachmentFormatString, String.Join(",", attachmentsStrings))

            logSimple "SlackInteractiveResponse.ToString(): before String.Format"
            let res = String.Format("{{ \"text\": \"{0}\", \"attachments\": [{1}]}}", this.text, attachments)
            logSimple("SlackInteractiveResponse.ToString(): res is\n" + res)
            res

type State = 
    {
        conversations: ConcurrentBag<Conversation>
        activeDialogues: ConcurrentDictionary<string, Dialogue>
        incomingWebhook: string
    }

let buildInteractiveResponse(choices: string seq) = 
    {
        SlackInteractiveResponse.text = "choice"
        buttonPrompt = "Make your choice"
        fallback = "None"
        callbackId = "none"
        color = "#7F7F7F"
        actions = choices |> Seq.mapi (fun i c -> { SlackInteractiveAction.name = i.ToString(); text = c; value = i.ToString() }) |> Array.ofSeq
    }

let advanceComputerSpeakingIfNecessary(state: State, d: Dialogue, channel: string): unit = 
    // if the dialogue is in the ComputerSpeaking state, set up an async handler to advance it to the
    // next state after a moment.
    match d.s with
    | DialogueState.ComputerSpeaking -> 
        logSimple "in advanceComputerSpeakingIfNecessary, DialogueState.ComputerSpeaking branch"
        async {
            logSimple "in async handler, before sleep"
            do! Async.Sleep(1000)
            logSimple "in async handler, after sleep"
            let (newDialogue2, result2, choices2) = runDialogue(d, "")
            logSimple "in async handler, after runDialogue"
            state.activeDialogues.TryUpdate(channel, newDialogue2, d) |> ignore
            logSimple "in async handler, after TryUpdate"

            // send the request to the webhook
            let response = buildInteractiveResponse(choices2)
            logSimple "in async handler, after buildInteractiveResponse"
            logSimple(response.ToString())
            logSimple "in async handler, after logging response.ToString()"
            //let response = "{\"text\": \"" + result2 + "\"}"
            Console.WriteLine(response.ToString())
            Http.RequestString(state.incomingWebhook, httpMethod = "POST", body = TextRequest(response.ToString()), headers = [ HttpRequestHeaders.ContentType(HttpContentTypes.Json) ]) |> ignore
        } |> Async.StartAsTask |> ignore
    | _ ->
        logSimple "in advanceComputerSpeakingIfNecessary, catch-all branch"
        ()    

let newDialogueHandler(state: State, name: string, channel: string, uid: string) = 
    match (state.conversations |> Seq.tryFind (fun c -> c.name = name)) with
    | Some(c) ->
        let d = 
            {
                Dialogue.c = c
                n = None
                s = DialogueState.Started
            }

        let mutable existingDialogue: Dialogue = Dialogue.Empty
        match state.activeDialogues.TryGetValue(channel, &existingDialogue) with
        | true when existingDialogue.s <> DialogueState.Terminated ->
            { SlackResponse.responseType = InChannel; text = "Dialogue [" + name + "] already in progress" }
        | s ->
            // start the dialogue automatically after 1 second
            async {
                do! Async.Sleep(1000)
                let (newDialogue, result, choices) = runDialogue(d, "")

                if (not s) then 
                    state.activeDialogues.TryAdd(channel, newDialogue) |> ignore
                else
                    state.activeDialogues.TryUpdate(channel, newDialogue, existingDialogue) |> ignore

                // send the request to the webhook
                let response = "{\"text\": \"" + result + "\"}"
                Http.RequestString(state.incomingWebhook, httpMethod = "POST", body = TextRequest(response.ToString()), headers = [ HttpRequestHeaders.ContentType(HttpContentTypes.Json) ]) |> ignore

                do! Async.Sleep(1000)
                advanceComputerSpeakingIfNecessary(state, newDialogue, channel)                
            } |> Async.StartAsTask |> ignore
            { SlackResponse.responseType = InChannel; text = "Started new dialogue [" + name + "]" }
    | _ -> { SlackResponse.responseType = Ephemeral; text = "Couldn't find dialogue named [" + name + "]" }

let dialogueRequestHandler(state: State)(r: SlackRequest): SlackResponse = 
    let cmdOpt = 
        r.Text |> Option.map (fun t -> t.Split([| ' ' |]) |> List.ofArray)

    match (cmdOpt, r.ChannelId, r.UserId) with
    | (Some([cmd; dialogueName]), Some(channel), Some(userId)) when cmd = "newDialogue" ->
        newDialogueHandler(state, dialogueName, channel, userId)
    | (Some(cmd :: tail), Some(channel), Some(_)) when cmd = "run" ->
        let d = state.activeDialogues.Item(channel)
        let (newDialogue, result, choices) = runDialogue(d, String.Join(" ", tail))
        state.activeDialogues.TryUpdate(channel, newDialogue, d) |> ignore
        advanceComputerSpeakingIfNecessary(state, newDialogue, channel)
        { SlackResponse.responseType = InChannel; text = result }
    | _ -> { SlackResponse.responseType = Ephemeral; text = "Unrecognized command or something else failed" }

let buttonRequestHandler(state: State)(r: SlackButtonRequest): SlackResponse = 
    let cmdOpt = Some(["run"; r.ActionValue |> Option.orDefault("")])
    logSimple("buttonRequestHandler(): r = " + r.ToString())
    match (cmdOpt, r.ChannelId, r.UserId) with
    | (Some(cmd :: tail), Some(channel), Some(_)) when cmd = "run" ->
        let d = state.activeDialogues.Item(channel)
        let (newDialogue, result, choices) = runDialogue(d, String.Join(" ", tail))
        state.activeDialogues.TryUpdate(channel, newDialogue, d) |> ignore
        advanceComputerSpeakingIfNecessary(state, newDialogue, channel)
        { SlackResponse.responseType = InChannel; text = result }
    | _ -> { SlackResponse.responseType = Ephemeral; text = "Unrecognized button action" }

let dialogueHandler(state: State)(ctx: HttpContext) = 
    (
        logSimple "in dialogueHandler"
        let resp = 
            SlackRequest.FromHttpContext ctx
            |> (dialogueRequestHandler(state))

        resp.ToString()
        |> OK
    ) ctx

let buttonHandler(state: State)(ctx: HttpContext) = 
    (
        let resp = 
            SlackButtonRequest.FromHttpContext ctx
            |> (buttonRequestHandler(state))

        resp.ToString()
        |> OK
    ) ctx

let echoHandler(ctx: HttpContext) = 
    (
        logSimple "in echoHandler"
        SlackRequest.FromHttpContext ctx
        |> fun req ->
            let resp = 
                match req.Text with
                | Some(t) when not(String.IsNullOrEmpty(t)) -> { SlackResponse.responseType = InChannel; text = t }
                | _ -> { SlackResponse.responseType = Ephemeral; text = "You didn't specify anything to echo." }

            resp.ToString()
            |> OK
    ) ctx

let app(state: State) = 
    let handler = dialogueHandler(state)
    let bh = buttonHandler(state)
    choose [
        POST >=> path "/echo" >=> handler >=> Writers.setMimeType "application/json"
        POST >=> path "/button" >=> bh >=> Writers.setMimeType "application/json"
    ]

//////////////////////////////
// Testing content
//////////////////////////////
let insertTestContent(state: State) = 
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
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.Parse(argv)

    // global state of available conversations, and all dialogues that have been started.
    let state = 
        { 
            State.conversations = new ConcurrentBag<Conversation>()
            activeDialogues = new ConcurrentDictionary<string, Dialogue>()
            incomingWebhook = results.GetResult(<@ IncomingWebhookUrl @>)
        }

    insertTestContent(state)
    startWebServer defaultConfig (app(state))
    0
    