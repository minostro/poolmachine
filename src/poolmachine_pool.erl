-module(poolmachine_pool).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
  supervisor:start_link({local, pool_name(Name)}, ?MODULE, [Name]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Name]) ->
  SupFlags = {one_for_one,1, 5},
  PoolManagerSpec = {
    poolmachine_pool_manager,
    {poolmachine_pool_manager, start_link, [Name]},
    temporary,
    brutal_kill,
    worker,
    []
  },
  WorkerSupSpec = {
    poolmachine_worker_sup,
    {poolmachine_worker_sup, start_link, [Name]},
    temporary,
    brutal_kill,
    worker,
    []
  },
  {ok, {SupFlags, [PoolManagerSpec, WorkerSupSpec]}}.

pool_name(Name) ->
  list_to_atom(Name ++ "_pool").