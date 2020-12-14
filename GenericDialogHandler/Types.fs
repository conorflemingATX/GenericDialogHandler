module BotbuilderDialogHandler.Types

(**
    ## Error Handling

    Errors can be handled in a few ways:
    Either handle it within the abstract methods with fallbacks, and outputting messages to the user.
    Or, let the whole thing fail and go to the adapter error handler.
    Or, wrap RunDialog in a try catch... Either way I don't think we have to handle errors in the DH.
*)

// Simply for readability
type AsyncResult<'A, 'E> = Async<Result<'A, 'E>>

// 'TC = TurnContext, 'A = Activity, 'C = Command, 'E = Error type
module State =
    (**
        I am returning a Result from the run and handleCommand methods.
        Ordinarily, you would expect that any errors should be handled through dialog by the Chatbot.
        Really, it should only return Error when faced with a major unrecoverable error.
        It will return a tuple of the error and the last good state (Just in case.)
    *)
    [<AbstractClass>]
    type DialogState<'TC, 'A, 'C, 'E>(ac: 'A list) =
        member val ActivityList = ac with get, set

        abstract Run: 'C option -> AsyncResult<DialogState<'TC, 'A, 'C, 'E> * 'C list, 'E>

        abstract HandleCommand: 'C -> AsyncResult<DialogState<'TC, 'A, 'C, 'E> * 'C list, 'E>

open State

// 'TC = TurnContext, 'A = Activity, 'C = Command, 'E = Error type
module DialogHandler =
    type GetCommands<'TC, 'C, 'E> = 'TC -> AsyncResult<'C list, 'E>
    type GetState<'TC, 'A, 'C, 'E> = 'TC -> AsyncResult<DialogState<'TC, 'A, 'C, 'E>, 'E>
    type SaveState<'TC, 'A, 'C, 'E> = 'TC -> DialogState<'TC, 'A, 'C, 'E> -> AsyncResult<unit, 'E>

    type RunDialog<'TC, 'A, 'C, 'E> =
        'TC -> AsyncResult<'A list, 'E * DialogState<'TC, 'A, 'C, 'E> option * 'C list option>

    type InitHandler<'TC, 'A, 'C, 'E> =
        GetCommands<'TC, 'C, 'E> -> GetState<'TC, 'A, 'C, 'E> -> SaveState<'TC, 'A, 'C, 'E> -> RunDialog<'TC, 'A, 'C, 'E>
