-module(poolmachine_test).

-export([start/0]).

start() ->
  proper:module(task_proper),
  proper:module(pool_worker_proper),
  proper:module(task_statem),
  proper:module(pool_statem).
