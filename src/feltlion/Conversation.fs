module Conversation

open System

type ConversationSpeaker = 
    | User of int seq   // if non-empty, specific slack user IDs that this node is restricted to
    | Computer

type ConversationNode =
    {
        text: string
        speaker: ConversationSpeaker
        next: ConversationNode seq
    }

type Conversation = 
    {
        startNodes: ConversationNode seq
        name: string
    }
    with
        static member Empty = 
            {
                Conversation.startNodes = []
                name = ""
            }

type DialogueState = 
    | Started
    | ComputerSpeaking
    | WaitingForResponse
    | Terminated

// Running instance of a conversation.
type Dialogue = 
    {
        c: Conversation

        // current node in the conversation
        n: ConversationNode option

        s: DialogueState
    }
    with
        static member Empty = 
            {
                Dialogue.c = Conversation.Empty
                n = None
                s = DialogueState.Started
            }

let runDialogue(d: Dialogue, i: string): (Dialogue * string * string seq) = 
    match d.s with
    | Started ->
        // pick first available node
        match d.c.startNodes |> Seq.tryHead with
        | Some(start) -> ({ d with s = ComputerSpeaking; n = Some(start) }, "Going to first node", Seq.empty)
        | _ -> ({ d with s = Terminated }, "[dialogue ended]", Seq.empty)
    | ComputerSpeaking ->
        match (d.n |> Option.map (fun n -> (n.text, n.next |> Seq.map(fun p -> p.text)))) with
        | Some((t, choices)) ->
            // build choice text
            let choiceText = 
                String.Join("\n", choices |> Seq.mapi (fun n t -> (n + 1).ToString() + ") " + t))

            match choices |> Seq.isEmpty with
            | true -> ({d with s = Terminated }, t + "\n" + choiceText, Seq.empty)
            | false -> ({ d with s = WaitingForResponse }, t + "\n" + choiceText, choices)
        | _ -> ({ d with s = Terminated }, "[dialogue ended]", Seq.empty)
    | WaitingForResponse ->
        let choice = Int32.Parse(i)
        let nextOpt = 
            d.n 
            |> Option.bind(fun n -> n.next |> Seq.tryItem(choice - 1))
        match (nextOpt) with
        | Some(n) -> ({ d with s = ComputerSpeaking; n = n.next |> Seq.tryHead }, n.text, Seq.empty)
        | _ -> ({ d with s = Terminated }, "[dialogue ended]", Seq.empty)
    | Terminated -> (d, "[dialogue ended]", Seq.empty)
