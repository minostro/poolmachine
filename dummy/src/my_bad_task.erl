-module(my_bad_task).
-behaviour(poolmachine_task_behaviour).

-export([call/1, on_success/2, on_error/3]).

call([]) ->
  {ok, 1/0}.

on_success(Result, RespondTo) ->
  RespondTo ! {success, Result}.

on_error(Error, 0, RespondTo) ->
  RespondTo ! {error, Error};
on_error(_Error, _RetriesRemaining, _RespondTo) ->
  ok.
