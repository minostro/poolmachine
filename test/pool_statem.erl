-module(pool_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, next_state/3, precondition/2, postcondition/3]).

-define(POOL_NAME, test_pool).
-define(POOL_MANAGER_NAME, test_pool_manager).
-define(PROXY_NAME, test_proxy).

-define(CAPACITIES, [0, 1, 2]).
-define(TASK_MODULES, [good_task]).
-define(INITIAL_CAPACITY, infinity).

%%% generators
task(#{proxy_pid := Pid}) ->
  ?LET({TaskModule, RunningMode}, {task_module(), running_mode()},
    poolmachine:new_task(#{module => TaskModule, args => [1], respond_to => Pid, running_mode => RunningMode})).

capacity() ->
  elements(?CAPACITIES).

task_module() ->
  elements(?TASK_MODULES).

running_mode() ->
  elements([async, sync]).

%%% Abstract state machine for Task
initial_state() ->
  #{
    capacity => ?INITIAL_CAPACITY,
    manager_pid => ?POOL_MANAGER_NAME,
    proxy_pid => ?PROXY_NAME,
    enqueued_tasks => []
  }.

command(#{manager_pid := Pid} = State) ->
  frequency([
    {5, {call, poolmachine_pool_manager, run, [Pid, task(State)]}},
    {1, {call, poolmachine_pool_manager, change_pool_size, [Pid, capacity()]}}
  ]).

precondition(_, _) ->
  true.

next_state(#{capacity := Capacity, enqueued_tasks := EnqueuedTasks} = State, _Var, {call, poolmachine_pool_manager, run, [_PoolName, Task]}) ->
  case Capacity of
    0 -> State#{enqueued_tasks => [Task | EnqueuedTasks]};
    _ -> State
  end;
next_state(State, _Var, {call, poolmachine_pool_manager, change_pool_size, [_PoolName, Capacity]}) ->
  State#{capacity => Capacity}.

postcondition(State, {call, poolmachine_pool_manager, run, [_PoolName, Task]}, Result) -> 
  equals(Result, ok),
  RunningMode = poolmachine_task:running_mode(Task),
  handle_postcondition_run(State, RunningMode, Task);
postcondition(#{capacity := Capacity, enqueued_tasks := Tasks, proxy_pid := Pid}, {call, poolmachine_pool_manager, change_pool_size, [_PoolName, NewCapacity]}, Result) ->
  equals(Result, ok),
  case [Capacity, NewCapacity] of
    [_, 0] -> true;
    [0, _] -> ensure_enqueued_tasks_were_ran(Pid, Tasks);
    [_, _] -> true
  end.

handle_postcondition_run(#{capacity := 0, proxy_pid := Pid}, sync, Task) ->
  TestResult = proxy(Pid, result, Task),
  TestResult =:= {error, "There are no workers available at the moment."};
handle_postcondition_run(#{proxy_pid := Pid}, sync, Task) ->
  TestResult = proxy(Pid, result, Task),
  case poolmachine_task:module(Task) of
    good_task -> (TestResult =:= {error, "There are no workers available at the moment."}) or (TestResult =:= {success, [[1], "test"]});
    bad_task ->
      case TestResult =:= {error, "There are no workers available at the moment."} of
        true -> true;
        false ->
          {error, {ExceptionType, ExceptionReason, _StackTrace}} = TestResult,
          [ExceptionType, ExceptionReason] =:= [error, badarith]
      end
  end;
handle_postcondition_run(#{capacity := 0}, async, _Task) ->
  true;
handle_postcondition_run(#{proxy_pid := Pid}, _Mode, Task) ->
  TestResult = proxy(Pid, result, Task),
  case poolmachine_task:module(Task) of
    good_task -> TestResult =:= {success, [[1], "test"]};
    bad_task  ->
      case TestResult =:= {error, "There are no workers available at the moment."} of
        true -> true;
        false ->
          {error, {ExceptionType, ExceptionReason, _StackTrace}} = TestResult,
          [ExceptionType, ExceptionReason] =:= [error, badarith]
      end
  end.

ensure_enqueued_tasks_were_ran(_ProxyPid, []) ->
  true;
ensure_enqueued_tasks_were_ran(ProxyPid, [Task | Rest]) ->
  RunningMode = poolmachine_task:running_mode(Task),
  handle_postcondition_run(#{proxy_pid => ProxyPid}, RunningMode, Task),
  ensure_enqueued_tasks_were_ran(ProxyPid, Rest).

prop_pool() ->
  ?FORALL(Cmds, commands(?MODULE),
    ?TRAPEXIT(
      begin
        init(),
        {H,S,Res} = run_commands(?MODULE, Cmds),
        cleanup(),
        ?WHENFAIL(
          io:format("History: ~w\n\nState: ~w\n\nRes: ~w\n",
            [H, S, Res]),
          aggregate(command_names(Cmds), Res =:= ok))
      end
    )
  ).

%%% Helper Functions
%%% FIXME: This is crazy stuff. Unfortunately, I don't know
%%% how to do this in a better way :(
init() ->
  pool(start),
  proxy(start).

cleanup() ->
  pool(stop),
  proxy(stop).

pool(start) ->
  {ok, PoolPid} = poolmachine_pool:start_link(self(), ?POOL_MANAGER_NAME, #{max_pool_size => ?INITIAL_CAPACITY}),
  PoolManagerPid = receive
    {register_pool, ?POOL_MANAGER_NAME, Pid} -> Pid
  after 500 ->
    error
  end,
  register(?POOL_NAME, PoolPid),
  register(?POOL_MANAGER_NAME, PoolManagerPid);
pool(stop) ->
  exit(whereis(?POOL_NAME), shutdown),
  unregister(?POOL_MANAGER_NAME). 

proxy(start) ->
  Pid = spawn(fun() -> loop(#{}) end),
  register(?PROXY_NAME, Pid);
proxy(stop) ->
  ?PROXY_NAME ! stop,
  unregister(?PROXY_NAME).

proxy(Pid, result, Task) ->
  %%% Waiting a little bit, so workers are done.
  timer:sleep(5),
  Pid ! {result, self(), poolmachine_task:ref(Task)},
  receive
    Msg -> Msg
  after 5 ->
    error
  end.

loop(State) ->
  receive
    {TaskRef, Result} ->
      loop(State#{TaskRef => Result});
    {result, From, TaskRef} ->
      Result = maps:get(TaskRef, State, no_result),
      From ! Result,
      loop(State);
    stop ->
      stop
  end.

