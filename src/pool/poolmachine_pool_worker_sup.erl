-module(poolmachine_pool_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1, start_child/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
  supervisor:start_link({local, worker_sup_name(Name)}, ?MODULE, []).

start_child(Name) ->
  supervisor:start_child(worker_sup_name(Name), []).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = {simple_one_for_one,1, 5},
  TaskServerSpec = {
    poolmachine_pool_worker,
    {poolmachine_pool_worker, start_link, []},
    temporary,
    brutal_kill,
    worker,
    []
  },
  {ok, {SupFlags, [TaskServerSpec]}}.

worker_sup_name(Name) ->
  list_to_atom(Name ++ "_worker_sup").