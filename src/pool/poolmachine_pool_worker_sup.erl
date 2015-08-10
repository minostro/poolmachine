-module(poolmachine_pool_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1, start_child/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link(?MODULE, []).

start_child(Pid) ->
  supervisor:start_child(Pid, []).
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
