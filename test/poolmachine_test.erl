-module(poolmachine_test).

-export([start/0]).

start() ->
  Results = [
    proper:module(task_proper),
    proper:module(pool_worker_proper),
    %proper:module(task_statem),
    proper:module(pool_statem)
  ],
  case lists:flatten(Results) of
    [] -> erlang:halt(0);
    _ -> erlang:halt(1)
  end.
