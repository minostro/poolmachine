-module(poolmachine_task).

-export([new/3]).

-type poolmachine_task() :: {MFA::mfa(), RespondTo::pid()}.
-export_type([poolmachine_task/0]).

new(Module, Arguments, RespondTo) ->
  new({Module, call, Arguments}, RespondTo).

new({_, _, _} = MFA, RespondTo) ->
  {MFA, RespondTo}.
  