-module(poolmachine_task_behaviour).

-callback init() -> {ok, any()} | {error, any()}.
-callback call(Args :: list(term()), RespondTo :: pid(), Task :: any()) -> tuple() | any().