module BotbuilderDialogHandler.Tests

open Xunit
open FsUnit.Xunit

module Utils =
    open BotbuilderDialogHandler.Types

    (**
        Mock TurnContext type
    *)
    type TurnContext = | TurnContext

    (**
        Mock Error Type
     *)
    type MockErr = | MockErr

    (**
        Mock Activity type is alias for int
    *)
    type Activity = int

    (**
        Mock Command type
    *)
    type Command =
        | AddCountToActivityListAndEmitNewCountCommand of int
        | AddCountToActivityListAndEmitStopProcessingCommand of int
        | StopProcessingCommand

    (**
        Mock State type
    *)
    open BotbuilderDialogHandler.Types.State

    type MockState() =
        inherit DialogState<TurnContext, int, Command, MockErr>([])

        let mutable count = 5

        override x.Run(cmd: Command option) =
            async { return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), []) }

        override x.HandleCommand (cmd: Command) =
            match cmd with
                | AddCountToActivityListAndEmitNewCountCommand n ->
                    async {
                        let len = x.ActivityList.Length
                        do x.ActivityList <- x.ActivityList @ [n]

                        let y = count
                        count <- count + 1

                        return if len < 2 then
                                  Ok ((x :> DialogState<TurnContext, int, Command, MockErr>),
                                    [ AddCountToActivityListAndEmitNewCountCommand y ])
                               else
                               Ok ((x :> DialogState<TurnContext, int, Command, MockErr>),
                                [ AddCountToActivityListAndEmitStopProcessingCommand y ])
                    }
                    
                | AddCountToActivityListAndEmitStopProcessingCommand n ->
                    async {
                        do x.ActivityList <- x.ActivityList @ [n]
                        return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), [ StopProcessingCommand ])
                    }
                    
                | StopProcessingCommand -> async { return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), []) }


    type MockFailureState() =
        inherit DialogState<TurnContext, int, Command, MockErr>([])

        let mutable count = 5

        override x.Run(cmd: Command option) =
            async { return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), []) }

        override x.HandleCommand (cmd: Command) =
            match cmd with
                | AddCountToActivityListAndEmitNewCountCommand n ->
                    async {
                        do x.ActivityList <- x.ActivityList @ [n]

                        let y = count
                        count <- count + 1
                        
                        return if n < 2 then
                                  Ok ((x :> DialogState<TurnContext, int, Command, MockErr>),
                                    [ AddCountToActivityListAndEmitNewCountCommand y ])
                               else
                                  Error MockErr
                    }
                    
                | AddCountToActivityListAndEmitStopProcessingCommand n ->
                    async {
                        do x.ActivityList <- n :: x.ActivityList
                        return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), [ StopProcessingCommand ])
                    }
                    
                | StopProcessingCommand -> async { return Ok ((x :> DialogState<TurnContext, int, Command, MockErr>), []) }


    (**
       This is a quick mock that allows me to pass the methods into the dialog handler
       init function and then store some state describing what was called and with what.
    *)

    type DialogHandlerDependencyMock() =
        let commands = [
            AddCountToActivityListAndEmitNewCountCommand 1;
            AddCountToActivityListAndEmitNewCountCommand 2;
            AddCountToActivityListAndEmitNewCountCommand 3;
            AddCountToActivityListAndEmitNewCountCommand 4;
        ]

        member val GetCommandsWasCalled : bool = false with get, set
        member val GetStateWasCalled : bool = false with get, set
        member val SaveStateWasCalled : bool = false with get, set

        // Mock implementation of dependencies.
        member x.GetCommands (tc: TurnContext) = async {
            do x.GetCommandsWasCalled <- true
            return Ok commands
        }
        
        member x.GetState (tc: TurnContext) = async {
            do x.GetStateWasCalled <- true
            return (new MockState () :> DialogState<TurnContext, int, Command, MockErr>) |> Ok
        }

        member x.GetFailingState (tc: TurnContext) = async {
            do x.GetStateWasCalled <- true
            return (new MockFailureState () :> DialogState<TurnContext, int, Command, MockErr>) |> Ok
        }
        
        member x.SaveState (tc: TurnContext) (s: DialogState<TurnContext, int, Command, MockErr>) = async {
            do x.SaveStateWasCalled <- true
            return Ok ()
        }

        member x.FailingGetCommands (tc: TurnContext) = async {
            do x.GetCommandsWasCalled <- true
            return Error MockErr
        }

        member x.FailingGetState (tc: TurnContext) = async {
            do x.GetStateWasCalled <- true
            return Error MockErr
        }

       
        member x.FailingSaveState (tc: TurnContext) (s: DialogState<TurnContext, int, Command, MockErr>) = async {
            do x.SaveStateWasCalled <- true
            return Error MockErr
        }

open Utils

[<Fact>]
let ``Should get commands, init and save state then resolve to Ok of activityList`` () =
    
    let mh = new DialogHandlerDependencyMock ()
    let dh =
        BotbuilderDialogHandler.Main.initHandler<TurnContext, int, Command, MockErr> mh.GetCommands mh.GetState mh.SaveState

    let result = dh Utils.TurnContext |> Async.RunSynchronously
    let allFunctionsCalled = mh.GetCommandsWasCalled && mh.GetStateWasCalled && mh.SaveStateWasCalled

    (match result with
     | Ok [1; 5; 6; 7; 2; 8; 3; 9; 4; 10] -> ()
     | _ -> failwith "Invalid") |> should not' shouldFail
    allFunctionsCalled |> should be True

[<Fact>]
let ``Should resolve to Error of 'E type when getCommands fails`` () =
    let mh = new DialogHandlerDependencyMock ()
    let dh =
        BotbuilderDialogHandler.Main.initHandler<TurnContext, int, Command, MockErr> mh.FailingGetCommands mh.GetState mh.SaveState
        
    let result = dh Utils.TurnContext |> Async.RunSynchronously

    (match result with
        | Error (MockErr, None, None) -> ()
        | _ -> failwith "Invalid") |> should not' shouldFail

[<Fact>]
let ``Should resolve to Error of 'E type when getState fails`` () =
    let mh = new DialogHandlerDependencyMock ()
    let dh =
        BotbuilderDialogHandler.Main.initHandler<TurnContext, int, Command, MockErr> mh.GetCommands mh.FailingGetState mh.SaveState
        
    let result = dh Utils.TurnContext |> Async.RunSynchronously
    let onlyGetCommandsAndGetStateWasCalled = mh.GetStateWasCalled && mh.GetCommandsWasCalled && (not mh.SaveStateWasCalled)

    (match result with
        | Error (MockErr, None, None) -> ()
        | _ -> failwith "Invalid") |> should not' shouldFail
    onlyGetCommandsAndGetStateWasCalled |> should be True

[<Fact>]
let ``Should resolve to Error of 'E type when saveState fails`` () =
    let mh = new DialogHandlerDependencyMock ()
    let dh =
        BotbuilderDialogHandler.Main.initHandler<TurnContext, int, Command, MockErr> mh.GetCommands mh.GetState mh.FailingSaveState
        
    let result = dh Utils.TurnContext |> Async.RunSynchronously
    let allFunctionsCalled = mh.GetCommandsWasCalled && mh.GetStateWasCalled && mh.SaveStateWasCalled

    (match result with
        | Error (MockErr, None, None) -> ()
        | _ -> failwith "Invalid") |> should not' shouldFail
    allFunctionsCalled |> should be True

[<Fact>]
let ``Should stop processing commands when HandleCommand fails, resolve to tuple of err, state and remaining commands`` () =
    let commandsLeft = [ 2; 3; 4; ] |> List.map (AddCountToActivityListAndEmitNewCountCommand)
    let mh = new DialogHandlerDependencyMock ()
    let dh =
       BotbuilderDialogHandler.Main.initHandler<TurnContext, int, Command, MockErr> mh.GetCommands mh.GetFailingState mh.SaveState
        
    let result = (dh Utils.TurnContext) |> Async.RunSynchronously

    match result with
        | Ok _ ->
            (false |> should be True)
        | Error (err, Some s, Some cs) ->
            err |> should equal MockErr
            s.ActivityList |> should equal [1; 5]
            cs |> should equal commandsLeft
        | _ ->
            (false |> should be True)
