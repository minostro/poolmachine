-module(poolmachine_pool_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  pool_sup_pid => pid(),
  keep_workers_alive => boolean(),
  max_pool_size => infinity | non_neg_integer(),
  pool_worker_sup_pid => undefined | pid(),
  workers => map()
}.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/3, schedule/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(PoolSupPid, Name, Properties) ->
  gen_server:start_link(?MODULE, [PoolSupPid, Name, Properties], []).

schedule(Pid, Task) ->
  gen_server:cast(Pid, {call, Task}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([PoolSupPid, PoolName, Properties]) ->
  KeepWorkersAlive = proplists:get_value(keep_workers_alive, Properties, false),
  MaxPoolSize = proplists:get_value(max_pool_size, Properties, infinity),
  self() ! {start_pool_worker_sup},
  self() ! {register_pool, PoolName},
  State = #{
    pool_sup_pid => PoolSupPid,
    keep_workers_alive => KeepWorkersAlive,
    max_pool_size => MaxPoolSize,
    pool_worker_sup_pid => undefined,
    workers => #{}
  },
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({call, Task}, #{pool_worker_sup_pid := SupPid, workers := Workers} = State) ->
  {ok, {Ref, NewTask}} = run_worker(SupPid, Task),
  {noreply, State#{workers => Workers#{Ref => NewTask}}}.

handle_info({start_pool_worker_sup}, #{pool_sup_pid := PoolSupPid} =  State) ->
  {ok, Pid} = start_pool_worker_sup(PoolSupPid),
  {noreply, State#{pool_worker_sup_pid => Pid}};
handle_info({register_pool, PoolName}, State) ->
  poolmachine_controller:register_pool(PoolName, self()),
  {noreply, State};
handle_info({'DOWN', MonitorRef, process, _WorkerPid, DownReason}, #{workers := Workers} = State) ->
  case maps:is_key(MonitorRef, Workers) of
    true ->
      handle_worker_down(MonitorRef, DownReason, State);
    false ->
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
start_pool_worker_sup(SupPid) ->
  {ok, Pid} = poolmachine_pool:start_worker_sup(SupPid),
  link(Pid),
  {ok, Pid}.

run_worker(SupPid, Task) ->
  {ok, Pid} = poolmachine_pool_worker_sup:start_child(SupPid),
  Ref = monitor(process, Pid),
  NewTask = poolmachine_task:increase_attempt(Task),
  poolmachine_pool_worker:run(Pid, async, NewTask),
  {ok, {Ref, NewTask}}.

handle_worker_down(_, normal, State) ->
  {noreply, State};
handle_worker_down(MonitorRef, DownError, #{pool_worker_sup_pid := SupPid, workers := Workers} = State) ->
  #{MonitorRef := Task} = Workers,
  NewWorkers = maps:remove(MonitorRef, Workers),
  case poolmachine_task:can_be_retried(Task) of
    true ->
      {ok, {Ref, NewTask}} = run_worker(SupPid, Task),
      erlang:display(DownError),
      erlang:display(NewTask),
      {noreply, State#{workers => maps:put(Ref, NewTask, NewWorkers)}};
    false ->
      erlang:display(":-("),
      {noreply, State#{workers => NewWorkers}}
  end.
