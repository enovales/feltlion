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

let runDialogue(d: Dialogue, i: string): (Dialogue * string) = 
    match d.s with
    | Started ->
        // pick first available node
        match d.c.startNodes |> Seq.tryHead with
        | Some(start) -> ({ d with s = ComputerSpeaking; n = Some(start) }, "Going to first node")
        | _ -> ({ d with s = Terminated }, "[dialogue ended]")
    | ComputerSpeaking ->
        match (d.n |> Option.map (fun n -> (n.text, n.next |> Seq.map(fun p -> p.text)))) with
        | Some((t, choices)) ->
            // build choice text
            let choiceText = 
                String.Join("\n", choices |> Seq.mapi (fun n t -> (n + 1).ToString() + ") " + t))
            ({ d with s = WaitingForResponse }, t + "\n" + choiceText)
        | _ -> ({ d with s = Terminated }, "[dialogue ended]")
    | WaitingForResponse ->
        let choice = Int32.Parse(i)
        let nextOpt = 
            d.n 
            |> Option.bind(fun n -> n.next |> Seq.tryItem(choice - 1))
        match (nextOpt) with
        | Some(n) -> ({ d with s = ComputerSpeaking; n = n.next |> Seq.tryHead }, n.text)
        | _ -> ({ d with s = Terminated }, "[dialogue ended]")
    | Terminated -> (d, "[dialogue ended]")
