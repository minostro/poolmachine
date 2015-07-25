-module(poolmachine_pool).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1, schedule/2]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name) ->
  supervisor:start_link({local, Name}, ?MODULE, []).

%TODO: Once we add the controller process for the pool
%this method has to be moved there.
schedule(Name, Task) ->
  {ok, Pid} = supervisor:start_child(Name, [Task]),
  poolmachine_worker:cast(Pid, perform),
  {ok, Pid}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  % SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
  % TaskServerSpec = #{
  %   id        => poolmachine_worker,
  %   start     => {poolmachine_worker, start_link, []},
  %   restart   => temporary,
  %   shutdown  => brutal_kill,
  %   type      => worker,
  %   modules   => []
  % },
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
