Pool Machine
=====

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

Sequence Chart
-----
![Sequence Chart](https://github.com/minostro/poolmachine/blob/master/docs/sequence_chart.png "Sequence Chart")


Acknowledgements
=====
I would like to thanks [francisco rojas](https://github.com/frojasg) for the ideas and discussions around this topic.  
