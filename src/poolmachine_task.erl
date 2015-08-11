-module(poolmachine_task).

-export([new/3, mfa/2, set/3, increase_attempt/1, can_be_retried/1]).

-type poolmachine_task() :: #{
  'module' => atom(),
  'call_args' => list(any()),
  'respond_to' => pid(),
  'max_retries' => integer(),
  'attempts' => integer(),
  'client_result' => any(),
  'client_error' => any()
}.
-export_type([poolmachine_task/0]).

new(Module, CallArgs, RespondTo) ->
  %TODO: max_retries can be consumed from application env or from the pool
  #{
    module => Module,
    call_args => CallArgs,
    respond_to => RespondTo,
    max_retries => 6,
    attempts => 0,
    client_result => undefined,
    client_error => undefined
  }.

mfa(#{module := Module, call_args := Args}, call) ->
  {Module, call, [Args]};
mfa(#{module := Module, client_result := Args, respond_to := RespondTo}, on_success) ->
  {Module, on_success, [Args, RespondTo]};
mfa(#{module := Module, client_error := Args, respond_to := RespondTo, attempts := Attempts, max_retries := MaxRetries}, on_error) ->
  RetriesRemaining = MaxRetries - Attempts,
  {Module, on_error, [Args, RetriesRemaining, RespondTo]}.

set(Task, client_result, Data) ->
  maps:update(client_result, Data, Task);
set(Task, client_error, Data) ->
  maps:update(client_error, Data, Task).


increase_attempt(#{attempts := Attempts} = Task) ->
  Task#{attempts => Attempts + 1}.

can_be_retried(#{attempts := Attempts, max_retries := MaxRetries} = Task) ->
  Attempts < MaxRetries.
