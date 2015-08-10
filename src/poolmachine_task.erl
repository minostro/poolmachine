-module(poolmachine_task).

-export([new/3, mfa/2, set/3]).

-type poolmachine_task() :: #{
  'module' => atom(),
  'call_args' => list(any()),
  'respond_to' => pid(),
  'max_retries' => integer(),
  'attempts' => integer(),
  'client_data' => any(),
  'client_result' => any()
}.
-export_type([poolmachine_task/0]).

new(Module, CallArgs, RespondTo) ->
  %TODO: max_retries can be possible be consumed from application env or from the pool
  #{
    module => Module,
    call_args => CallArgs,
    respond_to => RespondTo,
    max_retries => 6,
    attempts => 0,
    client_data => undefined,
    client_result => undefined
  }.

mfa(#{module := Module}, initialize) ->
  {Module, initialize, []};
mfa(#{module := Module, call_args := Args, client_data := ClientData}, call) ->
  {Module, call, [Args, ClientData]};
mfa(#{module := Module, client_result := Args, respond_to := RespondTo}, on_success) ->
  {Module, on_success, [Args, RespondTo]}.

set(Task, client_data, Data) ->
  maps:update(client_data, Data, Task);
set(Task, client_result, Data) ->
  maps:update(client_result, Data, Task).
