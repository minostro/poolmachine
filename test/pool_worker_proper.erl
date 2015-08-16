-module(pool_worker_proper).
-include_lib("proper/include/proper.hrl").

prop_run_good_task() ->
  ?FORALL({Module, Args}, {good_task, list(any())},
    begin
      Task = poolmachine_task:new(#{module => Module, args => Args, respond_to => self()}),
      TaskRef = poolmachine_task:ref(Task),

      {ok, Pid} = poolmachine_pool_worker:start(),
      poolmachine_pool_worker:run(Pid, Task),

      TestResult = receive
        {TaskRef, {success, Result}} -> Result
      after 100 ->
        error
      end,

      poolmachine_pool_worker:stop(Pid, normal),
      %%Please check tasks/good_task.erl, line 7
      TestResult =:=  [Args] ++ ["test2"]
    end).

prop_run_bad_task() ->
  ?FORALL({Module, Args}, {bad_task, list(any())},
    begin
      Task = poolmachine_task:new(#{module => Module, args => Args, respond_to => self()}),
      {ok, Pid} = poolmachine_pool_worker:start(),
      TaskRef = poolmachine_task:ref(Task),

      poolmachine_pool_worker:run(Pid, Task),

      {TestExceptionType, TestExceptionReason, _TestStackTrace} = receive
        {TaskRef, {error, Error}} -> Error
      after 100 ->
        error
      end,

      TestExceptionType =:= error,
      TestExceptionReason =:= badarith
    end).