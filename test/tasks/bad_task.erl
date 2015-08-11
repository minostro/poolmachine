-module(bad_task).
-behaviour(poolmachine_task_behaviour).

-export([call/1, on_success/2, on_error/3]).

call(_Arguments) ->
  {ok, 1/0}.

on_success(Result, RespondTo) ->
  RespondTo ! {success, Result}.

on_error(Error, _RetriesRemaining, RespondTo) ->
  RespondTo ! {error, Error}.
