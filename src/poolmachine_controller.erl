-module(poolmachine_controller).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-type state() :: #{
  pools => map()
}.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, start_pool/2, schedule/2, run/2, change_pool_size/2]).

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

schedule(PoolName, AsyncTask) ->
  gen_server:cast(?SERVER, {run, PoolName, AsyncTask}).

run(PoolName, Task) ->
  Ref = poolmachine_task:ref(Task),
  SyncTask = poolmachine_task:running_mode(Task, sync),
  gen_server:cast(?SERVER, {run, PoolName, SyncTask}),
  receive
    {Ref, Message} ->
      Message
  after 5000 ->
    timeout
  end.

change_pool_size(PoolName, Size) ->
  gen_server:cast(?SERVER, {pool_size, PoolName, Size}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
  {ok, #{pools => #{}}}.

handle_call({start_pool, PoolName, Properties}, _From, State) ->
  poolmachine_pool_sup:start_child(self(), PoolName, Properties),
  {reply, ok, State}.

handle_cast({run, PoolName, Task}, State) ->
  Pid = pool_manager_pid(PoolName, State),
  poolmachine_pool_manager:run(Pid, Task),
  {noreply, State};
handle_cast({pool_size, PoolName, Size}, State) ->
  Pid = pool_manager_pid(PoolName, State),
  poolmachine_pool_manager:change_pool_size(Pid, Size),
  {noreply, State}.

handle_info({register_pool, PoolName, Pid}, #{pools := Pools} = State) ->
  {noreply, State#{pools => Pools#{PoolName => Pid}}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
pool_manager_pid(PoolName, #{pools := Pools}) ->
  maps:get(PoolName, Pools).