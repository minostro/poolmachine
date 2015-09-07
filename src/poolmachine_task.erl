-module(poolmachine_task).

-export([new/1, mfa/2, can_be_retried/1]).

%%% Getters API definition
-export([attempts/1, client_result/1, client_error/1, ref/1, respond_to/1, running_mode/1, module/1]).

%%% Setters API definition
-export([client_result/2, client_error/2, increase_attempt/1, running_mode/2]).

-type poolmachine_task() :: #{
  'ref' => reference(),
  'module' => atom(),
  'args' => list(any()),
  'respond_to' => pid(),
  'max_retries' => integer(),
  'attempts' => integer(),
  'client_result' => any(),
  'client_error' => any(),
  'running_mode' => async | sync
}.
-export_type([poolmachine_task/0]).

new(Args) ->
  %TODO: max_retries can be consumed from application env or from the pool
  Attrs = #{
    ref => make_ref(),
    max_retries => 6,
    attempts => 0,
    client_result => undefined,
    client_error => undefined,
    running_mode => async
  },
  maps:merge(Attrs, Args).


%%% getters implementation
attempts(#{attempts := Attempts}) ->
  Attempts.
client_result(#{client_result := ClientResult}) ->
  ClientResult.
client_error(#{client_error := ClientError}) ->
  ClientError.
ref(#{ref := TaskRef}) ->
  TaskRef.
respond_to(#{respond_to := RespondTo}) ->
  RespondTo.
running_mode(#{running_mode := RunningMode}) ->
  RunningMode.
module(#{module := Module}) ->
  Module.

%%% Setters implementation
client_result(Task, Value) ->
  maps:update(client_result, Value, Task).
client_error(Task, Value) ->
  maps:update(client_error, Value, Task).
running_mode(Task, Value) ->
  maps:update(running_mode, Value, Task).
increase_attempt(#{attempts := Attempts} = Task) ->
  maps:update(attempts, Attempts + 1, Task).

can_be_retried(#{attempts := Attempts, max_retries := MaxRetries, running_mode := async}) ->
  Attempts < MaxRetries;
can_be_retried(#{running_mode := sync}) ->
  false.

mfa(#{module := Module, args := Args}, call) ->
  {Module, call, [Args]};
mfa(#{module := Module, client_result := Args, ref := TaskRef, respond_to := RespondTo}, on_success) ->
  {Module, on_success, [Args, TaskRef, RespondTo]};
mfa(#{module := Module, client_error := Args, ref := TaskRef, respond_to := RespondTo} = Task, on_error) ->
  RetriesRemaining = retries_remaining(Task),
  {Module, on_error, [Args, TaskRef, RespondTo, #{retries_remaining => RetriesRemaining}]}.


%%% Private methods
retries_remaining(#{attempts := Attempts, max_retries := MaxRetries, running_mode := async}) ->
  MaxRetries - Attempts;
retries_remaining(#{running_mode := sync}) ->
  0.
