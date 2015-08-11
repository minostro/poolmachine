-module(task_properties).
-include_lib("proper/include/proper.hrl").

pid() ->
  ?LET({First, Second, Third}, {0, nat(), nat()},
    begin
      Pid = "<" ++ integer_to_list(First) ++ "." ++ integer_to_list(Second) ++ "." ++ integer_to_list(Third) ++ ">",
      list_to_pid(Pid)
    end).

task() ->
  ?LET({Module, Args, RespondTo}, {atom(), list(any()), pid()},
     {Module, Args, RespondTo, poolmachine_task:new(Module, Args, RespondTo)}).

prop_call_mfa() ->
  ?FORALL({Module, Args, RespondTo, Task}, task(),
    begin
      Task = poolmachine_task:new(Module, Args, RespondTo),
      poolmachine_task:mfa(Task, call) =:= {Module, call, [Args]}
    end).

prop_on_success_mfa() ->
  ?FORALL({{Module, _Args, RespondTo, Task}, ClientResult}, {task(), any()},
    begin
      NewTask = poolmachine_task:set(Task, client_result, ClientResult),
      poolmachine_task:mfa(NewTask, on_success) =:= {Module, on_success, [ClientResult, RespondTo]}
    end).

prop_on_error_mfa() ->
  ?FORALL({{Module, _Args, RespondTo, Task}, Error}, {task(), any()},
    begin
      NewTask = poolmachine_task:set(Task, client_error, Error),
      poolmachine_task:mfa(NewTask, on_error) =:= {Module, on_error, [Error, 6, RespondTo]}
    end).

prop_get_client_result() ->
  ?FORALL({{_Module, _Args, _RespondTo, Task}, ClientResult}, {task(), any()},
    begin
      NewTask = poolmachine_task:set(Task, client_result, ClientResult),
      ClientResult =:= poolmachine_task:get(NewTask, client_result)
    end).

prop_can_be_retried() ->
  ?FORALL({{_Module, _Args, _RespondTo, Task}, N}, {task(), nat()},
    begin
      NewTask = increase_attempts(Task, N),
      CanBeRetried = poolmachine_task:can_be_retried(NewTask),
      case N >= 6 of
        true ->
          CanBeRetried =:= false;
        false ->
          CanBeRetried =:= true
      end
    end).

increase_attempts(Task, 0) ->
  Task;
increase_attempts(Task, N) ->
  increase_attempts(poolmachine_task:increase_attempt(Task), N-1).
