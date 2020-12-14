module BotbuilderDialogHandler.Main

(**
    ## Implementation

    1. Get the previous state from the persistence layer, whatever that means...
    and get the commands from the input using the TurnContext. Both of these are Async,
    and both of these can be run in parallel.

    2. Fold all commands into the State. For every Command in the Command list that
    was returned from the GetCommand method run the command handler and collect both the
    new state and any additional commands that may have been generated. If new Commands
    are produced, then append them to the list and keep going until the list is empty.
    Return the new state to serve as the next prompt state.

    3. Save the state to the backend using the SaveState function.

    4. Finally, return the ActivityList containing all the accululated outputs to be
    sent to the user.
*)

(**
    Note that as commands are run, they can also produce new commands which are
    appended to the command list, "(commands @ moreCommands)"
 *)

open BotbuilderDialogHandler.Types.State

(**
         BIG QUESTION !!!:
         Should we add additional commands produced by the handler to the end of the list?
         Or should we process new commands first before going back to processing others?
         Right now I am leaning towards the second.
         *)

let rec processCommandsToCompletion (currentState: DialogState<'TC, 'A, 'C, 'E>) (commandList: 'C list) =
    async {
        match commandList with
        | [] -> return Ok currentState
        | command :: commands ->
            let! res = currentState.HandleCommand command

            match res with
            | Ok (newState, moreCommands) ->
                return! processCommandsToCompletion newState (moreCommands @ commands)
            | Error err -> return Error(err, Some currentState, Some commands)
    }


let initHandler<'TC, 'A, 'C, 'E> (getCommands: ('TC -> Async<Result<'C list, 'E>>))
                                 (getState: ('TC -> Async<Result<DialogState<'TC, 'A, 'C, 'E>, 'E>>))
                                 (saveState: ('TC -> DialogState<'TC, 'A, 'C, 'E> -> Async<Result<unit, 'E>>))
                                 (tc: 'TC)
                                 =
    async {
        let! commandList = getCommands tc
        let! state = getState tc

        let (state', commandList') =
            (state
             |> Result.mapError (fun err -> (err, None, None))),
            (commandList
             |> Result.mapError (fun err -> (err, None, None)))

        let x =
            match state', commandList' with
            | Ok s, Ok cl -> Ok(processCommandsToCompletion s cl)
            | Error sErr, Error _ -> Error((sErr): ('E * DialogState<'TC, 'A, 'C, 'E> option * 'C list option))
            | Error sErr, Ok _ -> Error((sErr): ('E * DialogState<'TC, 'A, 'C, 'E> option * 'C list option))
            | Ok s, Error clErr -> Error((clErr): ('E * DialogState<'TC, 'A, 'C, 'E> option * 'C list option))

        let! (state'': Result<DialogState<'TC, 'A, 'C, 'E>, 'E * DialogState<'TC, 'A, 'C, 'E> option * 'C list option>) =
            async {
                match x with
                | Ok res ->
                    let! res' = res
                    return res'
                | Error r -> return Error r
            }

        return! async {
                    match state'' with
                    | Ok st ->
                        let al = st.ActivityList
                        let! saveResult = saveState tc st

                        match saveResult with
                        | Ok _ -> return Ok al
                        | Error err -> return Error(err, None, None)
                    | Error err -> return Error err
                }

    }
