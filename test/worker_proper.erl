-module(worker_proper).
-include_lib("proper/include/proper.hrl").

prop_delete() ->
  ?FORALL({X,L}, {integer(),list(integer())},
    not lists:member(X, lists:delete(X,L))).