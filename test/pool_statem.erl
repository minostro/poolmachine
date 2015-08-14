-module(pool_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, next_state/3, precondition/2, postcondition/3]).

-define(POOL_NAME, test_pool).
-define(POOL_MANAGER_NAME, test_pool_manager).
-define(PROXY_NAME, test_proxy).

-define(CAPACITIES, [0, 1, 2]).
-define(TASK_MODULES, [good_task, bad_task]).
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
    {4, {call, poolmachine_pool_manager, run, [Pid, task(State)]}},
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
  Result =:= ok,
  RunningMode = poolmachine_task:running_mode(Task),
  handle_postcondition_run(State, RunningMode, Task);
postcondition(#{capacity := Capacity, enqueued_tasks := Tasks, proxy_pid := Pid}, {call, poolmachine_pool_manager, change_pool_size, [_PoolName, _Capacity]}, Result) ->
  Result =:= ok,
  case Capacity of
    0 -> ensure_enqueued_tasks_were_ran(Pid, Tasks);
    _ -> true
  end.

handle_postcondition_run(#{capacity := 0, proxy_pid := Pid}, sync, Task) ->
  TestResult = proxy(Pid, result, Task),
  TestResult =:= {error, "There are no workers available at the moment."};
handle_postcondition_run(#{proxy_pid := Pid}, sync, Task) ->
  TestResult = proxy(Pid, result, Task),
  case TestResult of
    {error, "There are no workers available at the moment."} -> true;
    {success, [[1], "test"]} -> true;
    {error, {error, badarith, _}} -> true
  end;
handle_postcondition_run(#{capacity := 0}, async, _Task) ->
  true;
handle_postcondition_run(#{proxy_pid := Pid}, _Mode, Task) ->
  TestResult = proxy(Pid, result, Task),
  case {poolmachine_task:module(Task), TestResult} of
    {good_task, {success, [[1], "test"]}} -> true;
    {bad_task, {error, {error, badarith, _}}} -> true
  end.

ensure_enqueued_tasks_were_ran(_ProxyPid, []) ->
  true;
ensure_enqueued_tasks_were_ran(ProxyPid, [Task | Rest]) ->
  TestResult = proxy(ProxyPid, result, Task),
  TestResult =:= {success, [[1], "test"]},
  ensure_enqueued_tasks_were_ran(ProxyPid, Rest).

prop_task() ->
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
%%% This is crazy stuff. Unfortunately, I don't how to do this better :(
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
  timer:sleep(3),
  Pid ! {result, self(), poolmachine_task:ref(Task)},
  receive
    Msg -> Msg
  after 1 ->
    error
  end.

loop(State) ->
  receive
    {TaskRef, Result} ->
      loop(State#{TaskRef => Result});
    {result, From, TaskRef} ->
      Result = maps:get(TaskRef, State, undefined),
      From ! Result,
      loop(State);
    stop ->
      stop
  end.

