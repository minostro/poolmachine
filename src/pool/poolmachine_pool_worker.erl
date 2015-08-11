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
-export([start_link/0, start/0, run/3, stop/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  gen_server:start_link(?MODULE, [], []).

start() ->
  gen_server:start(?MODULE, [], []).

run(Pid, sync, Task) ->
  gen_server:call(Pid, {run, Task});
run(Pid, async, Task) ->
  gen_server:cast(Pid, {run, Task}).

stop(Pid, Reason) ->
  gen_server:cast(Pid, {stop, Reason}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  {ok, undefined}.

handle_call({run, Task}, _From, State) ->
  NewTask = run(call, Task),
  Result = poolmachine_task:get(NewTask, client_result),
  {reply, {ok, Result}, State}.

handle_cast({run, Task}, State) ->
  try run(call, Task) of
    NewTask -> run(on_success, NewTask),
    {stop, normal, State}
  catch
    Exception:Reason ->
      Error = {Exception, Reason, erlang:get_stacktrace()},
      NewTask = poolmachine_task:set(Task, client_error, Error),
      run(on_error, NewTask),
      {stop, Error, State}
  end;
handle_cast({stop, Reason}, State) ->
  {stop, Reason, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
run(call, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, call),
  {ok, Result} = apply(M, F, A),
  poolmachine_task:set(Task, client_result, Result);
run(on_success, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, on_success),
  apply(M, F, A);
run(on_error, Task) ->
  {M, F, A} = poolmachine_task:mfa(Task, on_error),
  apply(M, F, A).
