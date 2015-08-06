-module(poolmachine).

-export([start_link/0, start_pool/2, new_task/2, schedule/2]).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  poolmachine_sup:start_link().

-spec start_pool(atom(), list({atom(), any()})) -> {ok, pid()} | {error, any()}.
start_pool(Name, Properties) ->
  poolmachine_pool_sup:start_pool(Name, Properties).

-spec new_task(atom(), list(any())) -> poolmachine_task:task().
new_task(Module, CallArguments) ->
  poolmachine_task:new(Module, CallArguments, self()).

-spec schedule(atom(), poolmachine_task:task()) -> {ok, pid()} | {error, any()}.
schedule(PoolName, Task) ->
  poolmachine_pool_manager:schedule(PoolName, Task).
