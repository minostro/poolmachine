-module(poolmachine_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_pool(PoolName) ->
  supervisor:start_child(?SERVER, [PoolName]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = {simple_one_for_one,1, 5},
  PoolSpec = {
    poolmachine_pool,
    {poolmachine_pool, start_link, []},
    temporary,
    brutal_kill,
    worker,
    []
  },
  {ok, {SupFlags, [PoolSpec]}}.
