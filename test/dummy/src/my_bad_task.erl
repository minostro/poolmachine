-module(my_bad_task).
-behaviour(poolmachine_task_behaviour).

-export([initialize/0, call/2, on_success/2, on_error/2]).

initialize() ->
  {ok, my_test}.

call([], _State) ->
  {ok, 1/0}.

on_success(Result, RespondTo) ->
  RespondTo ! {success, Result}.

on_error(Error, RespondTo) ->
  RespondTo ! {error, Error}.
