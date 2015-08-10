-module(poolmachine_pool_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  pool_name => string(),
  keep_workers_alive => boolean(),
  max_pool_size => infinity | non_neg_integer()
}.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2, schedule/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Name, Properties) ->
  gen_server:start_link({local, pool_manager_name(Name)}, ?MODULE, [Name, Properties], []).

%TODO: we might want to move into the process itself when we want to
%restrict the amount of workers or keep a reference
schedule(Name, Task) ->
  gen_server:cast(pool_manager_name(Name), {call, Task}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Name, Properties]) ->
  KeepWorkersAlive = proplists:get_value(keep_workers_alive, Properties, false),
  MaxPoolSize = proplists:get_value(max_pool_size, Properties, infinity),
  {ok, #{pool_name => Name, keep_workers_alive => KeepWorkersAlive, max_pool_size => MaxPoolSize}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({call, Task}, #{pool_name := Name, keep_workers_alive := KeepWorkerAlive} = State) ->
  {ok, Pid} = poolmachine_pool_worker_sup:start_child(Name),
  poolmachine_pool_worker:cast(Pid, {initialize, Task, KeepWorkerAlive}),
  poolmachine_pool_worker:cast(Pid, call),
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
pool_manager_name(Name) ->
  list_to_atom(Name ++ "_pool_manager").

