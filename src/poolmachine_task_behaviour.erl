-module(poolmachine_task_behaviour).

-callback call(Args :: list(term())) -> any().
-callback on_success(Result :: term(), TaskRef :: reference(), RespondTo :: pid()) -> any().
-callback on_error(Error :: {atom(), atom(), erlang:stack_item()}, TaskRef :: reference(), RespondTo :: pid(), Data :: map()) -> any().