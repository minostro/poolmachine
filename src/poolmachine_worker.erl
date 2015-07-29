-module(poolmachine_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, cast/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Task) ->
  gen_server:start_link(?MODULE, [Task], []).

cast(Pid, Message) ->
  gen_server:cast(Pid, Message).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Task]) ->
  {ok, Result} = run_init_function(Task),
  {ok, poolmachine_task:update(Task, client_data, Result)}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(perform, Task) ->
  run_call_function(Task),
  handle_worker_done(Task);
handle_cast(_Msg, State) ->
  {noreply, State}.

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

run_call_function(Task) ->
  apply(maps:get(module, Task), call, maps:get(call_args, Task)).

handle_worker_done(Task) ->
  handle_worker_done(Task, maps:get(keep_worker_alive, Task)).

handle_worker_done(Task, true) ->
  {noreply, Task};
handle_worker_done(Task, false) ->
  {stop, normal, Task}.
