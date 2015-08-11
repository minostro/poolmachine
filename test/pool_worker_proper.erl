-module(pool_worker_proper).
-include_lib("proper/include/proper.hrl").

prop_sync_run_good_task() ->
  ?FORALL({Module, Args, RespondTo}, {good_task, list(any()), nat()},
    begin
      Task = poolmachine_task:new(Module, Args, RespondTo),
      {ok, Pid} = poolmachine_pool_worker:start(),
      {ok, Result} = poolmachine_pool_worker:run(Pid, sync, Task),
      poolmachine_pool_worker:stop(Pid, normal),
      Result =:=  [Args] ++ ["test"]
    end).

prop_async_run_good_task() ->
  ?FORALL({Module, Args}, {good_task, list(any())},
    begin
      Task = poolmachine_task:new(Module, Args, self()),
      {ok, Pid} = poolmachine_pool_worker:start(),
      poolmachine_pool_worker:run(Pid, async, Task),

      TestResult = receive
        {success, Result} -> Result
      after 100 ->
        error
      end,

      poolmachine_pool_worker:stop(Pid, normal),
      %%Please check tasks/good_task.erl, line 7
      TestResult =:=  [Args] ++ ["test"]
    end).

prop_sync_run_bad_task() ->
  ?FORALL({Module, Args, RespondTo}, {bad_task, list(any()), nat()},
    begin
      Task = poolmachine_task:new(Module, Args, RespondTo),
      {ok, Pid} = poolmachine_pool_worker:start(),

      {TestException, TestExceptionReason} = try
        poolmachine_pool_worker:run(Pid, sync, Task)
      catch
        Exception:Reason -> {Exception, Reason}
      end,

      {{TestReason, _}, _} = TestExceptionReason,
      TestException =:= exit,
      TestReason =:= badarith
    end).

prop_async_run_bad_task() ->
  ?FORALL({Module, Args}, {bad_task, list(any())},
    begin
      Task = poolmachine_task:new(Module, Args, self()),
      {ok, Pid} = poolmachine_pool_worker:start(),
      poolmachine_pool_worker:run(Pid, async, Task),

      {TestExceptionType, TestExceptionReason, _TestStackTrace} = receive
        {error, Error} -> Error
      after 100 ->
        error
      end,

      TestExceptionType =:= error,
      TestExceptionReason =:= badarith
    end).