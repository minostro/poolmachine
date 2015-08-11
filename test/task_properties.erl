-module(task_properties).
-include_lib("proper/include/proper.hrl").


prop_call_mfa() ->
  ?FORALL({Module, Args, RespondTo}, {atom(), list(any()), int()},
    begin
      Task = poolmachine_task:new(Module, Args, RespondTo),
      poolmachine_task:mfa(Task, call) =:= {Module, call, [Args]}
    end).
