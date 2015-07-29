-module(my_task).
-behaviour(poolmachine_task_behaviour).

-export([init/0, call/3]).

init() ->
  {ok, my_test}.

call(Arguments, RespondTo, State) ->
  erlang:display(Arguments),
  erlang:display(RespondTo),
  erlang:display(State).