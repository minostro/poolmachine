-module(task_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, next_state/3, precondition/2, postcondition/3]).

%%% Abstract state machine for Task
initial_state() ->
  poolmachine_task:new(#{module => my_module, args => [], respond_to => list_to_pid("<0.0.1>"), ref => list_to_pid("<0.0.1>")}).

command(Task) ->
  oneof([
    {call, poolmachine_task, client_error, [Task, any()]},
    {call, poolmachine_task, client_result, [Task, any()]},
    {call, poolmachine_task, mfa, [Task, on_error]},
    {call, poolmachine_task, mfa, [Task, on_success]},
    {call, poolmachine_task, increase_attempt, [Task]},
    {call, poolmachine_task, can_be_retried, [Task]}
  ]).

%%% Preconditions can also be defined in the command(Task) generator,
%%% by inspecting the state and using improper lists.
precondition(Task, {call, poolmachine_task, mfa, [_Task, on_error]}) ->
  poolmachine_task:client_error(Task) =/= undefined;
precondition(Task, {call, poolmachine_task, mfa, [_Task, on_success]}) ->
  poolmachine_task:client_result(Task) =/= undefined;
precondition(_, _) ->
  true.

next_state(Task, _Var, {call, poolmachine_task, client_error, [_Task, Value]}) ->
  poolmachine_task:client_error(Task, Value);
next_state(Task, _Var, {call, poolmachine_task, client_result, [_Task, Value]}) ->
  poolmachine_task:client_result(Task, Value);
next_state(Task, _Var, {call, poolmachine_task, increase_attempt, [_Task]}) ->
  poolmachine_task:increase_attempt(Task);
next_state(Task, _Var, {call, poolmachine_task, mfa, [_Task, on_error]}) ->
  Task;
next_state(Task, _Var, {call, poolmachine_task, mfa, [_Task, on_success]}) ->
  Task;
next_state(Task, _Var, {call, poolmachine_task, can_be_retried, [_Task]}) ->
  Task.


%%% You use state in the postcondition handler when the state is suppose to whole
%%% a dependency on your current state.  In this case, when being in the state mfa(Task, on_error)
%%% *prior* state (Task) is supposed to have client_error value.  Thus, you use your *prior* state
%%% to confirm that your current state is valid.

%%% How the world should be after executing this command?  You use the *prior* state to
%%% define your expections.  Let's assume that attempts was 0 before increase_attempt
%%% command execution. NewTask::Attempts should be 1.  So (0 + 1) =:= 1
postcondition(_State, {call,poolmachine_task,client_result, [_Task, Value]}, NewTask) ->
  Value =:= poolmachine_task:client_result(NewTask);
postcondition(_State, {call,poolmachine_task,client_error, [_Task, Value]}, NewTask) ->
  Value =:= poolmachine_task:client_error(NewTask);
postcondition(Task, {call, poolmachine_task, can_be_retried, [_Task]}, Result) ->
  Result =:= poolmachine_task:can_be_retried(Task);
postcondition(Task, {call, poolmachine_task, mfa, [_Task, on_error]}, MFA) ->
  poolmachine_task:mfa(Task, on_error) =:= MFA;
postcondition(Task, {call, poolmachine_task, mfa, [_Task, on_success]}, MFA) ->
  poolmachine_task:mfa(Task, on_success) =:= MFA;
postcondition(Task, {call, poolmachine_task, increase_attempt, [_Task]}, NewTask) ->
  poolmachine_task:attempts(Task) + 1 =:= poolmachine_task:attempts(NewTask).


prop_task() ->
  ?FORALL(Cmds, commands(?MODULE),
    ?TRAPEXIT(
      begin
        {H,S,Res} = run_commands(?MODULE, Cmds),
        ?WHENFAIL(
          io:format("History: ~w\n\nState: ~w\n\nRes: ~w\n",
            [H, S, Res]),
          aggregate(command_names(Cmds), Res =:= ok))
      end
    )
  ).
