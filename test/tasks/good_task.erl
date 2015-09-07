-module(good_task).
-behaviour(poolmachine_task_behaviour).

-export([call/2, on_success/3, on_error/4]).

call(Arguments, _State) ->
  {ok, [Arguments] ++ ["test"]}.

on_success(Result, TaskRef, RespondTo) ->
  RespondTo ! {TaskRef, {success, Result}}.

on_error(_Error, _TaskRef, _RespondTo, _Data) ->
  ok.
