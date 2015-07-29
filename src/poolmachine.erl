-module(poolmachine).

-export([start_link/0, start_pool/1, new_task/3, schedule/2]).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  poolmachine_sup:start_link().

-spec start_pool(atom()) -> {ok, pid()} | {error, any()}.
start_pool(Name) ->
  poolmachine_pool_sup:start_pool(Name).

-spec new_task(atom(), list(any()), pid()) -> poolmachine_task:task().
new_task(Module, CallArguments, RespondTo) ->
  poolmachine_task:new(Module, CallArguments, RespondTo).

-spec schedule(atom(), poolmachine_task:task()) -> {ok, pid()} | {error, any()}.
schedule(PoolName, Task) ->
  poolmachine_pool:schedule(PoolName, Task).
