-module(poolmachine_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  task => any(),
  keep_alive => boolean()
}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2, cast/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Task, KeepAlive) ->
  gen_server:start_link(?MODULE, [Task, KeepAlive], []).

cast(Pid, Message) ->
  gen_server:cast(Pid, Message).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Task, KeepAlive]) ->
  {ok, Result} = run_init_function(Task),
  UpdatedTask = poolmachine_task:update(Task, client_data, Result),
  {ok, #{task => UpdatedTask, keep_alive => KeepAlive}}.

handle_cast(perform, #{task := Task} = State) ->
  run_call_function(Task),
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
run_init_function(Task) ->
  apply(maps:get(module, Task), init, []).

run_call_function(#{call_args := CallArgs, respond_to := RespondTo, client_data := ClientData} = Task) ->
  apply(maps:get(module, Task), call, [CallArgs, RespondTo, ClientData]).

handle_worker_done(#{keep_alive := KeepAlive} = State) ->
  handle_worker_done(State, KeepAlive).

handle_worker_done(State, true) ->
  {noreply, State};
handle_worker_done(State, false) ->
  {stop, normal, State}.
