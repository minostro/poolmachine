-module(bad_task).
-behaviour(poolmachine_task_behaviour).

-export([call/2, on_success/3, on_error/4]).

call(_Arguments, _State) ->
  {ok, 1/0}.

on_success(Result, TaskRef, RespondTo) ->
  RespondTo ! {TaskRef, {success, Result}}.

on_error(Error, TaskRef, RespondTo, _Data) ->
  RespondTo ! {TaskRef, {error, Error}}.
