On the understanding about using "user-defined types" vs using primitive types together.

```erlang
-module(tasks).
-export([new/2, module/1, arguments/1, attempts/1]).

-opaque task() :: #{}.
-export_type(task/0).


new(Module, Arguments) ->
  #{module => Module, arguments => Arguments, attempts => 0}.

module(#{module := Module}) -> Module.
arguments(#{arguments := Arguments}) -> Arguments.
attempts(#{attempts := Attempts}) -> Attempts.

incr_attempts(#{attempts := Attempts} = Task) ->
  Task#{attempts = Attempts + 1}.
```

So clients that want to interact with this module has to do the following:

```erlang
do_something_with(Task) ->
  Module = tasks:module(Task),
  Arguments = tasks:arguments(Task),
  Attempts = tasks:attemtps(Task),

Task = tasks:new(subtract, {1, 2}),
do_something_with(Task).
```

The code above shows how clients loose the ability to destructure `Task` now because this is an opaque type.

Questions
---

1. Does this mean that `do_something_with` should be implemented in the `tasks` module?.
2. Does this mean that `do_something_with` should be implemented as a process cause that's how you model `activities` in Erlang?
3. By defining an user-defined type or ADT, clients lose the ability to destructure --do pattern matching-- `Task`.  A `Task` can only be destructured by its module (`tasks`).  This might seem obvious at first, but it took me a while to learn that.  The same situation happen in Erlang with its `Queue` implementation.