-module(test_eqc).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

prop_seq() ->
  ?FORALL({From, To}, {int(), int()},
          try List = lists:seq(From, To),
              length(List) == To - From + 1
          catch
            error: _ ->
              (To - From + 1) < 0
          end).