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
-export([start_link/1, start/1, run/2, stop/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(InitFunction) ->
  gen_server:start_link(?MODULE, [InitFunction], []).

start(InitFunction) ->
  gen_server:start(?MODULE, [InitFunction], []).

run(Pid, Task) ->
  gen_server:cast(Pid, {run, Task}).

stop(Pid, Reason) ->
  gen_server:cast(Pid, {stop, Reason}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([InitFunction]) ->
  self() ! {initizalize_worker, InitFunction},
  {ok, undefined}.

handle_cast({run, Task}, State) ->
  try perform(call, Task, State) of
    NewTask -> perform(on_success, NewTask, State),
    %%FIXME: If we want to take infinity out from here
    %%we should define a handle_info for timeouts.
    %%I'm not sure what I'm doing here :-)
    %%{noreply, State, infinity}
    {stop, normal, State}
  catch
    Exception:Reason ->
      Error = {Exception, Reason, erlang:get_stacktrace()},
      NewTask = poolmachine_task:client_error(Task, Error),
      perform(on_error, NewTask, State),
      {stop, Error, State}
  end;
handle_cast({stop, Reason}, State) ->
  {stop, Reason, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info({initizalize_worker, InitFunction}, _State) ->
  {ok, State} = InitFunction(),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
perform(call, Task, State) ->
  {M, F, A} = poolmachine_task:mfa(Task, call),
  {ok, Result} = apply(M, F, A ++ [State]),
  poolmachine_task:client_result(Task, Result);
perform(on_success, Task, _State) ->
  {M, F, A} = poolmachine_task:mfa(Task, on_success),
  apply(M, F, A);
perform(on_error, Task, _State) ->
  {M, F, A} = poolmachine_task:mfa(Task, on_error),
  apply(M, F, A).
