-module(poolmachine_pool_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, schedule/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link(Name) ->
  gen_server:start_link({local, pool_manager_name(Name)}, ?MODULE, [Name], []).

%TODO: we might want to move into the process itself when we want to
%restrict the amount of workers or keep a reference
schedule(Name, Task) ->
  gen_server:cast(pool_manager_name(Name), {perform, Task}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Name]) ->
  {ok, Name}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({perform, Task}, Name) ->
  poolmachine_worker_sup:start_child(Name, Task),
  {noreply, Name};
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
pool_manager_name(Name) ->
  list_to_atom(Name ++ "_pool_manager").