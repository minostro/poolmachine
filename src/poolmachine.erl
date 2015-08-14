-module(poolmachine).

-export([start/0, start_pool/2, new_task/1, schedule/2, run/2, change_pool_size/2]).

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
  application:start(poolmachine).

-spec start_pool(atom(), map()) -> {ok, pid()} | {error, any()}.
start_pool(Name, Properties) ->
  poolmachine_controller:start_pool(Name, Properties).

-spec new_task(map()) -> poolmachine_task:task().
new_task(Args) ->
  poolmachine_task:new(maps:merge(#{respond_to => self()}, Args)).

-spec schedule(atom(), poolmachine_task:task()) -> {ok, pid()} | {error, any()}.
schedule(PoolName, Task) ->
  poolmachine_controller:schedule(PoolName, Task).

-spec run(atom(), poolmachine_task:task()) -> {success, any()} | {pool_manager_error, any()}.
run(PoolName, Task) ->
  poolmachine_controller:run(PoolName, Task).

-spec change_pool_size(atom(), integer()) -> ok.
change_pool_size(PoolName, Size) ->
  poolmachine_controller:change_pool_size(PoolName, Size).
