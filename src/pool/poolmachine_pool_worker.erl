-module(poolmachine_pool_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  task => any(),
  keep_alive => boolean()
}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, cast/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

cast(Pid, Message) ->
  gen_server:cast(Pid, Message).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  {ok, undefined}.

handle_cast({initialize, Task, KeepAlive}, State) ->
  initialize_worker(Task, KeepAlive, State);
handle_cast(call, #{task := Task} = State) ->
  NewTask = run(call, Task),
  run(on_success, NewTask),
  handle_worker_done(State).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
initialize_worker(Task, KeepAlive, undefined) ->
  NewTask = run(initialize, Task),
  {noreply, #{task => NewTask, keep_alive => KeepAlive}};
initialize_worker(_Task, _KeepAlive, State) ->
  {noreply, State}.

run(initialize, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, initialize),
  {ok, Result} = apply(M, F, A),
  poolmachine_task:set(Task, client_data, Result);
run(call, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, call),
  {ok, Result} = apply(M, F, A),
  poolmachine_task:set(Task, client_result, Result);
run(on_success, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, on_success),
  apply(M, F, A).

handle_worker_done(#{keep_alive := true} = State) ->
  {noreply, State};
handle_worker_done(#{keep_alive := false} = State) ->
  {stop, normal, State}.
