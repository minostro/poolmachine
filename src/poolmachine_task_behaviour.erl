-module(poolmachine_task_behaviour).

-callback call(Args :: list(term())) -> any().
-callback on_success(Result :: term(), RespondTo :: pid()) -> any().
-callback on_error(Error :: {atom(), atom(), erlang:stack_item()}, RetriesRemaining :: integer(), RespondTo :: pid()) -> any().