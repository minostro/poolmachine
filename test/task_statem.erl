-module(task_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, next_state/3, precondition/2, postcondition/3]).

%%% Abstract state machine for Task
initial_state() ->
  poolmachine_task:new(my_module, [], self()).

command(Task) ->
  oneof([
    {call, poolmachine_task, set, [Task, client_error, nat()]},
    {call, poolmachine_task, mfa, [Task, on_error]}
  ]).

precondition(Task, {call, poolmachine_task, mfa, [_Task, on_error]}) ->
  poolmachine_task:get(Task, client_error) =/= undefined;
precondition(_, _) ->
  true.

next_state(Task, _Var, {call, poolmachine_task, set, [_Task, client_error, Value]}) ->
  poolmachine_task:set(Task, client_error, Value);
next_state(Task, _Var, {call, poolmachine_task, mfa, [_Task, on_error]}) ->
  Task.

%%% You use state in the postcondition handler when the state is suppose to whole
%%% a dependency on your current state.  In this case, when being in the state mfa(Task, on_error)
%%% *prior* state (Task) is supposed to have client_error value.  Thus, you use your *prior* state
%%% to confirm that your current state is valid.
postcondition(_Task, {call, poolmachine_task, set, [_Task, client_error, Value]}, NewTask) ->
  Value =:= poolmachine_task:get(NewTask, client_error);
postcondition(Task, {call, poolmachine_task, mfa, [_Task, on_error]}, MFA) ->
  poolmachine_task:mfa(Task, on_error) =:= MFA.


prop_task() ->
  ?FORALL(Cmds, commands(?MODULE),
    begin
      {H,S,Res} = run_commands(?MODULE, Cmds),
      ?WHENFAIL(
        io:format("History: ~w\nState: ~w\nRes: ~w\n",
          [H, S, Res]),
        aggregate(command_names(Cmds), Res =:= ok))
    end).