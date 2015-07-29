-module(poolmachine_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1, start_child/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
  supervisor:start_link({local, worker_sup_name(Name)}, ?MODULE, []).

start_child(Name, Task, KeepWorkerAlive) ->
  supervisor:start_child(worker_sup_name(Name), [Task, KeepWorkerAlive]).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = {simple_one_for_one,1, 5},
  TaskServerSpec = {
    poolmachine_worker,
    {poolmachine_worker, start_link, []},
    temporary,
    brutal_kill,
    worker,
    []
  },
  {ok, {SupFlags, [TaskServerSpec]}}.

worker_sup_name(Name) ->
  list_to_atom(Name ++ "_worker_sup").