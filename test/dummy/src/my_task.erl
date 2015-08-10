-module(my_task).
-behaviour(poolmachine_task_behaviour).

-export([initialize/0, call/2, on_success/2, on_error/2]).

initialize() ->
  {ok, my_test}.

call(Arguments, State) ->
  erlang:display(Arguments),
  erlang:display(State),
  {ok, 1}.

on_success(Result, RespondTo) ->
  erlang:display(Result),
  erlang:display(RespondTo),
  RespondTo ! {success, Result}.

on_error(Error, RespondTo) ->
  RespondTo ! {error, Error}.
