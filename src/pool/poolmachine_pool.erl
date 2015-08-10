-module(poolmachine_pool).

-behaviour(supervisor).

%% API
-export([start_link/2, start_worker_sup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name, Properties) ->
  supervisor:start_link(?MODULE, [Name, Properties]).

start_worker_sup(Pid) ->
  WorkerSupSpec = {
    poolmachine_pool_worker_sup,
    {poolmachine_pool_worker_sup, start_link, []},
    temporary,
    brutal_kill,
    supervisor,
    []
  },
  supervisor:start_child(Pid, WorkerSupSpec).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Name, Properties]) ->
  SupFlags = {one_for_all,1, 5},
  PoolManagerSpec = {
    poolmachine_pool_manager,
    {poolmachine_pool_manager, start_link, [self(), Name, Properties]},
    permanent,
    brutal_kill,
    worker,
    []
  },
  {ok, {SupFlags, [PoolManagerSpec]}}.
