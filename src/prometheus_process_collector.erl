-module(prometheus_process_collector).
-on_load(init/0).
-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2,
         get_process_info/0]).

-import(prometheus_model_helpers, [create_mf/5,
                                   counter_metrics/1,
                                   counter_metric/1,
                                   counter_metric/2,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   boolean_metric/1,
                                   boolean_metric/2,
                                   summary_metric/1,
                                   summary_metric/2,
                                   histogram_metric/1,
                                   histogram_metric/2,
                                   untyped_metric/1,
                                   untyped_metric/2]).

-behaviour(prometheus_collector).

-define(CHUNK_SIZE, 1024).

-define(APPNAME, prometheus_process_collector).
-define(LIBNAME, prometheus_process_collector).

-define(METRICS, [{process_open_fds, gauge,
                   "Number of open file descriptors."},
                  {process_max_fds, gauge,
                   "Maximum number of open file descriptors."},
                  {process_start_time_seconds, gauge,
                   "Start time of the process since unix epoch in seconds."},
                  {process_uptime_seconds, counter,
                   "Process uptime in seconds."},
                  {process_threads_total, gauge,
                   "Process Threads count."},
                  {process_virtual_memory_bytes, gauge,
                   "Virtual memory size in bytes."},
                  {process_resident_memory_bytes, gauge,
                   "Resident memory size in bytes."},
                  {process_cpu_seconds_total, counter,
                   "Process CPU seconds total.",
                   fun(Info) ->
                       counter_metrics([{[{kind, utime}],
                                         proplists:get_value(process_utime_seconds, Info)},
                                        {[{kind, stime}],
                                         proplists:get_value(process_stime_seconds, Info)}])
                   end},
                  {process_max_resident_memory_bytes, gauge,
                   "Maximum resident set size used."},
                  {process_noio_pagefaults_total, counter,
                   "Number of page faules serviced without any I/O activity."},
                  {process_io_pagefaults_total, counter,
                   "Number of page faults serviced that required I/O activity."},
                  {process_swaps_total, counter,
                   "Number of times a process was \"swapped\" out of main memory."},
                  {process_disk_reads_total, counter,
                   "Number of times the file system had to perform input."},
                  {process_disk_writes_total, counter,
                   "Number of times the file system had to perform output."},
                  {process_signals_delivered_total, counter,
                   "Number of signals delivered."},
                  {process_voluntary_context_switches_total, counter,
                   "Number of times a context switch resulted due to a "
                   "process voluntarily giving up the processor."},
                  {process_involuntary_context_switches_total, counter,
                   "Number of times a context switch resulted due to a "
                   "higher priority process becoming runnable or because the "
                   "current process exceeded its time slice."}
                 ]).

%% API exports
-export([]).

%%====================================================================
%% Collector API
%%====================================================================

deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
  ProcessInfo = get_process_info(),
  [mf(Callback, Metric, ProcessInfo) || Metric <- ?METRICS],
  ok.

collect_metrics(_, {Fun, Proplist}) ->
  Fun(Proplist).

mf(Callback, Metric, Proplist) ->
  {Name, Type, Help, Fun} = case Metric of
                              {Key, Type1, Help1} ->
                                {Key, Type1, Help1, fun (Proplist1) ->
                                                        metric(Type1, [], proplists:get_value(Key, Proplist1))
                                                    end};
                              {Key, Type1, Help1, Fun1} ->
                                {Key, Type1, Help1, Fun1}
                            end,
  Callback(create_mf(Name, Help, Type, ?MODULE, {Fun, Proplist})).

%%====================================================================
%% Private Parts
%%====================================================================

metric(counter, Labels, Value) ->
  counter_metric(Labels, Value);
metric(gauge, Labels, Value) ->
  gauge_metric(Labels, Value);
metric(summary, Labels, Value0) ->
  summary_metric(Labels, Value0);
metric(histogram, Labels, Value0) ->
  histogram_metric(Labels, Value0);
metric(untyped, Labels, Value) ->
  untyped_metric(Labels, Value);
metric(boolean, Labels, Value0) ->
  boolean_metric(Labels, Value0).

get_process_info() ->
  not_loaded(?LINE).

init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,

  erlang:load_nif(SoName, 0).

not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
