-module(poolmachine_controller).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  pools => map()
}.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, start_pool/2, register_pool/2, schedule/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_pool(Name, Properties) ->
  gen_server:call(?SERVER, {start_pool, Name, Properties}).

register_pool(Name, Pid) ->
  gen_server:cast(?SERVER, {pool_registered, Name, Pid}).

schedule(PoolName, Task) ->
  gen_server:call(?SERVER, {schedule, PoolName, Task}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  {ok, #{pools => #{}}}.

handle_call({start_pool, PoolName, Properties}, _From, State) ->
  poolmachine_pool_sup:start_child(PoolName, Properties),
  {reply, ok, State};
handle_call({schedule, PoolName, Task}, _From, #{pools := Pools} = State) ->
  #{PoolName := Pid} = Pools,
  poolmachine_pool_manager:schedule(Pid, Task),
  {reply, ok, State}.

handle_cast({pool_registered, PoolName, Pid}, #{pools := Pools} = State) ->
  {noreply, State#{pools => Pools#{PoolName => Pid}}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
