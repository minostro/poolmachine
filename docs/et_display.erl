-module(et_display).
-export([sequence_chart/0]).

sequence_chart() ->
  {ok, Viewer} = et_viewer:start([
    {title, "Client Schedule Task"},
    {trace_global,true},
    {trace_pattern,{et,max}},
    {max_actors,10}
  ]),
  Collector = et_viewer:get_collector_pid(Viewer),
  report_event(Collector, client, poolmachine, start_link, []),
  report_event(Collector, poolmachine, poolmachine_sup, start_link, []),
  report_event(Collector, client, poolmachine, start_pool, ['salesforce_pool']),
  report_event(Collector, poolmachine, pm_pool_sup, start_pool, ['salesforce_pool']),
  report_event(Collector, client, poolmachine, new_task, [module, [1, 2], 'pid()']),
  report_event(Collector, poolmachine, poolmachine_task, new, [module, call, [1, 2], 'pid()']),
  report_event(Collector, client, poolmachine, schedule, ['salesforce_pool', 'Task']),
  report_event(Collector, poolmachine, poolmachine_pool, schedule, ['salesforce_pool', 'Task']),
  report_event(Collector, poolmachine_pool, poolmachine_worker, cast, ['pid()', 'perform']).

report_event(Collector, From, To, Action, Inputs) ->
  Priority = 99,
  et_collector:report_event(Collector,Priority,From,To,Action,Inputs).
