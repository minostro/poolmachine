-module(poolmachine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  SupFlags = {one_for_one,1, 5},
  PoolSupSpec = {
    poolmachine_pool_sup,
    {poolmachine_pool_sup, start_link, []},
    permanent,
    brutal_kill,
    supervisor,
    []
  },
  PoolControllerSpec = {
    poolmachine_controller,
    {poolmachine_controller, start_link, []},
    permanent,
    brutal_kill,
    supervisor,
    []
  },
  {ok, {SupFlags, [PoolSupSpec, PoolControllerSpec]}}.
