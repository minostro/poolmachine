%%%-------------------------------------------------------------------
%% @doc dummy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('dummy_sup').

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

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	ChildSpec = {
		poolmachine,
    {poolmachine, start_link, []},
    permanent,
    brutal_kill,
    supervisor,
    []
	},
  {ok, {{one_for_all, 0, 1}, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
