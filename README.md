Pool Machine
=====

[<img src="http://quickcheck-ci.com/p/minostro/poolmachine.png" alt="Build Status" width="160px">](http://quickcheck-ci.com/p/minostro/poolmachine)

Yet Another Pool Application for Erlang projects.

How to use it
-----

Add Pool Machine to your deps:

```erlang
{deps, [
  {poolmachine, {git, "https://github.com/minostro/poolmachine.git", {branch, "master"}}}
]}.
```

After you have fetched the dependency, you have to add `poolmachine_sup` to your root supervisor:

```erlang
init([]) ->
  Poolmachine = {
    poolmachine_sup,
    {poolmachine_sup, start_link, []},
    permanent,
    5000,
    supervisor,
    []},
  {ok, {{one_for_one, 10, 20}, [Poolmachine, ...]} }.
```

Once your application is up and running, `poolmachine_sup` will automatically spawn a `default` pool for you.  You can schedule a
Task on the default pool by doing:

```erlang
Task = poolmanager:new_task(subtract, {2, 1}, self()),
poolmanager:schedule(Task),
flush(),
Shell got {ok, 1}
```

`new_task` receives a callback module (`subtract`), the arguments that will be used when running (calling `call`) the task (`{2, 1}`), and a pid to send the result back (`self()`).

You have to define the `subtract` module as follows:

```erlang
-module(substract).
-behaviour(poolmanager_task_behaviour).

call({N1, N2}, RespondTo) ->
  Result = N1 - N2,
  RespondTo ! {ok, Result}.

```

As you can see, the `subtract` module is in charge to communicate the result back.

Supervision Tree
-----
![Supervision Tree](https://github.com/minostro/poolmachine/blob/master/docs/supervision-tree.png "Supervision Tree")


Sequence Chart
-----
![Sequence Chart](https://github.com/minostro/poolmachine/blob/master/docs/sequence-chart.png "Sequence Chart")


New API
-----

```erlang
poolmachine:start_pool(salesforce, [{max_pool_size, infinity},
                        {keep_workers_alive, true}]).
Task = poolmachine:new_task('salesforce_task', self()).
poolmachine:schedule(salesforce, Task).
```

```erlang
-module('salesforce_task').
-behaviour(poolmachine_task_behaviour).

init() ->
  [SessionId, SessionToken] = ...
  {ok, [{session_id, SessionId}, {session_token, SessionToken}]}.

call(Arguments, RespondTo, State) ->
  do_some_stuff().
```

```erlang
-type poolmachine_task() :: #{
  'module' => atom(),
  'respond_to' => pid(),
  'max_retries' => integer(),
  'attempts' => integer(),
  'client_data' => any()
}.

```

Acknowledgements
=====
I would like to thanks [francisco rojas](https://github.com/frojasg) for the ideas and discussions around this topic.  
