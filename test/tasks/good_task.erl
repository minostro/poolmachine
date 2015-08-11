-module(good_task).
-behaviour(poolmachine_task_behaviour).

-export([call/1, on_success/2, on_error/3]).

call(Arguments) ->
  {ok, [Arguments] ++ ["test"]}.

on_success(Result, RespondTo) ->
  RespondTo ! {success, Result}.

on_error(_Error, _RetriesRemaining, _RespondTo) ->
  ok.
