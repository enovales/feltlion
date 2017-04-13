namespace Feltlion

// Conversation with generic node, choice, and state types.
type IConversation<'n, 'c, 's> = 
    abstract member Name: string with get
    abstract member Choices: 'n -> 'c seq
    abstract member GetStartNode: 's -> 'n
    // Used to advance the state of the conversation, by 
    // providing the current state and choice. The new node
    // is returned, or none if the conversation should be terminated.
    abstract member Next: ('s * 'c) -> 'n option

type ConversationState<'n, 'c, 's> = 
    {
        c: IConversation<'n, 'c, 's>
        position: 'n
    }