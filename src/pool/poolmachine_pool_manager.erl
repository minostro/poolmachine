-module(poolmachine_pool_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  pool_sup_pid => pid(),
  max_pool_size => infinity | non_neg_integer(),
  pool_worker_sup_pid => undefined | pid(),
  active_workers => map(),
  active_workers_count => 0,
  queue => list()
}.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/3, run/2, change_pool_size/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(PoolSupPid, Name, PropertyMap) ->
  gen_server:start_link(?MODULE, [PoolSupPid, Name, PropertyMap], []).

run(Pid, Task) ->
  gen_server:cast(Pid, {run, Task}).

change_pool_size(Pid, Size) ->
  gen_server:cast(Pid, {pool_size, Size}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([PoolSupPid, PoolName, PropertyMap]) ->
  self() ! {start_pool_worker_sup},
  self() ! {register_pool, PoolName},
  State = #{
    pool_sup_pid => PoolSupPid,
    max_pool_size => infinity,
    pool_worker_sup_pid => undefined,
    active_workers => #{},
    active_workers_count => 0,
    queue => []
  },
  {ok, maps:merge(State, PropertyMap)}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({run, Task}, State) ->
  RunningMode = poolmachine_task:running_mode(Task),
  handle_run_task(RunningMode, Task, State);
handle_cast({pool_size, Size}, #{max_pool_size := CurrentSize} = State) ->
  NewState = State#{max_pool_size => Size},
  case Size > CurrentSize of
    true ->
      {noreply, run_queued_tasks(NewState)};
    false ->
      {noreply, NewState}
  end.

handle_info({start_pool_worker_sup}, State) ->
  handle_start_pool_worker_sup(State);
handle_info({register_pool, PoolName}, State) ->
  poolmachine_controller:register_pool(PoolName, self()),
  {noreply, State};
handle_info({'DOWN', MonitorRef, process, _WorkerPid, DownReason}, #{active_workers := Workers} = State) ->
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
handle_run_task(async, Task, State) ->
  NewState = case workers_available(State) of
    true ->
      run_task(Task, State);
    false ->
      enqueue_task(Task, State)
  end,
  {noreply, NewState};
handle_run_task(sync, Task, State) ->
  NewState = case workers_available(State) of
    true ->
      run_task(Task, State);
    false ->
      notify_error(Task, out_of_capacity),
      State
  end,
  {noreply, NewState}.

handle_worker_down(MonitorRef, normal, State) ->
  NewState = remove_worker(MonitorRef, State),
  {noreply, run_queued_tasks(NewState)};
handle_worker_down(MonitorRef, _DownError, #{active_workers := Workers} = State) ->
  #{MonitorRef := Task} = Workers,
  NewState = remove_worker(MonitorRef, State),
  case poolmachine_task:can_be_retried(Task) of
    true ->
      RunningMode = poolmachine_task:running_mode(Task),
      handle_run_task(RunningMode, Task, NewState);
    false ->
      {noreply, NewState}
  end.

handle_start_pool_worker_sup(#{pool_sup_pid := PoolSupPid} = State) ->
  {ok, Pid} = poolmachine_pool:start_worker_sup(PoolSupPid),
  link(Pid),
  {noreply, State#{pool_worker_sup_pid => Pid}}.

run_task(Task, #{pool_worker_sup_pid := SupPid, active_workers := Workers, active_workers_count := Count} = State) ->
  {ok, Pid} = poolmachine_pool_worker_sup:start_child(SupPid),
  Ref = monitor(process, Pid),
  NewTask = poolmachine_task:increase_attempt(Task),
  poolmachine_pool_worker:run(Pid, NewTask),
  State#{active_workers => Workers#{Ref => NewTask}, active_workers_count => Count + 1}.

run_queued_tasks(#{queue := []} = State) ->
  State;
run_queued_tasks(#{queue := [Task, Rest]} = State) ->
  case workers_available(State) of
    true ->
      NewState = run_task(Task, State),
      run_queued_tasks(NewState#{queue => Rest});
    false ->
      State
  end.

workers_available(#{max_pool_size := MaxSize, active_workers_count := Count}) ->
  MaxSize > Count.

enqueue_task(Task, #{queue := Queue} = State) ->
  State#{queue => [Task, Queue]}.

remove_worker(MonitorRef, #{active_workers := Workers, active_workers_count := Count} = State) ->
  State#{active_workers => maps:remove(MonitorRef, Workers), active_workers_count := Count -1}.

notify_error(Task, out_of_capacity) ->
  RespondTo = poolmachine_task:respond_to(Task),
  TaskRef = poolmachine_task:ref(Task),
  RespondTo ! {TaskRef, {pool_manager_error, "There are not workers available at this moment."}}.
