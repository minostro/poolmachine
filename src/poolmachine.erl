-module(poolmachine).

-export([start_pool/1, new_task/3, schedule/1, schedule/2]).

-spec start_pool(atom()) -> {ok, pid()} | {error, any()}.
start_pool(Name) ->
  poolmachine_pool_sup:start_pool(Name).

-spec new_task(atom(), any(), pid()) -> poolmachine_task:task().
new_task(Module, Arguments, RespondTo) ->
  poolmachine_task:new(Module, Arguments, RespondTo).

-spec schedule(poolmachine_task:task()) -> {ok, pid()} | {error, any()}.
schedule(Task) ->
  schedule(default, Task).

-spec schedule(atom(), poolmachine_task:task()) -> {ok, pid()} | {error, any()}.
schedule(PoolName, Task) ->
  poolmachine_pool:schedule(PoolName, Task).
