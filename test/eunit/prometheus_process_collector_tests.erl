-module(prometheus_process_collector_tests).

-include_lib("eunit/include/eunit.hrl").

prometheus_format_test_() ->
  {foreach,
   fun prometheus_eunit_common:start/0,
   fun prometheus_eunit_common:stop/1,
   [fun test_process_collector/1]}.

test_process_collector(_) ->
  prometheus_process_collector:register(),
  Metrics = prometheus_text_format:format(),
  [
   ?_assertMatch({match, _}, re:run(Metrics, "process_open_fds [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_max_fds [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_start_time_seconds [0-9.]{10,}")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_uptime_seconds [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_threads_total [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_virtual_memory_bytes [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_resident_memory_bytes [1-9]")),
   ?_assertMatch({match, _}, re:run(Metrics, "process_cpu_seconds_total{kind=\"(utime|stime)\"} [0-9]"))
  ].
