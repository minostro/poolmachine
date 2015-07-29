-module(poolmachine_task).

-export([new/3, update/3]).

-type poolmachine_task() :: #{
  'module' => atom(),
  'call_args' => list(any()),
  'respond_to' => pid(),
  'max_retries' => integer(),
  'attempts' => integer(),
  'client_data' => any()
}.
-export_type([poolmachine_task/0]).

%TODO: max_retries can be possible be consumed from application env or from the pool
new(Module, CallArgs, RespondTo) ->
  #{module => Module, call_args => CallArgs, respond_to => RespondTo, max_retries => 6, attempts => 0, client_data => undefined}.

update(Task, Key, Value) ->
  maps:update(Key, Value, Task).
