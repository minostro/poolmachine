-module(poolmachine_task_behaviour).

-callback call(Args :: list(term()), RespondTo :: pid(), Task :: any()) -> tuple() | any().