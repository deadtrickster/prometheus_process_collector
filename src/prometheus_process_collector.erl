-module(prometheus_process_collector).
-on_load(init/0).
-export([register/0,
         register/1,
         deregister/1,
         collect_mf/2,
         collect_metrics/2]).

-import(prometheus_model_helpers, [create_mf/5,
                                   label_pairs/1,
                                   gauge_metrics/1,
                                   gauge_metric/1,
                                   gauge_metric/2,
                                   counter_metric/1,
                                   counter_metric/2]).

-behaviour(prometheus_collector).

-define(CHUNK_SIZE, 1024).

-define(APPNAME, prometheus_process_collector).
-define(LIBNAME, prometheus_process_collector).

%% API exports
-export([]).

%%====================================================================
%% Collector API
%%====================================================================

register() ->
  register(default).

register(Registry) ->
  ok = prometheus_registry:register_collector(Registry, ?MODULE).

deregister(_) -> ok.

collect_mf(Callback, _Registry) ->

  Stat = read_stat(),

  Callback(create_gauge(process_open_fds,
                        "Number of open file descriptors.",
                        [])),

  Callback(create_gauge(process_max_fds,
                        "Maximum number of open file descriptors.",
                        [])),

  Callback(create_gauge(process_start_time_seconds,
                        "Start time of the process since unix epoch in seconds.",
                        [])),

  Callback(create_gauge(process_uptime_seconds,
                        "Process uptime in seconds.",
                        [])),

  Callback(create_gauge(process_threads_total,
                        "Process Threads count.",
                        Stat)),

  Callback(create_gauge(process_virtual_memory_bytes,
                        "Virtual memory size in bytes.",
                        Stat)),

  Callback(create_gauge(process_resident_memory_bytes,
                        "Resident memory size in bytes.",
                        Stat)),

  %% Callback(create_gauge(process_heap_bytes,
  %%                       "Heap size.",
  %%                       Stat)),

  Callback(create_counter(process_cpu_seconds_total,
                          "Process CPU seconds total.",
                          Stat)).

collect_metrics(process_open_fds, _) ->
  gauge_metric(open_fds_count());
collect_metrics(process_max_fds, _) ->
  gauge_metric(max_fds_count());
collect_metrics(process_start_time_seconds, _) ->
  gauge_metric(start_time());
collect_metrics(process_uptime_seconds, _) ->
  gauge_metric(current_timestamp() - start_time());
collect_metrics(process_threads_total, Stat) ->
  gauge_metric(list_to_integer(lists:nth(20, Stat)));
collect_metrics(process_virtual_memory_bytes, Stat) ->
  gauge_metric(list_to_integer(lists:nth(23, Stat)));
collect_metrics(process_resident_memory_bytes, Stat) ->
  gauge_metric(sc_pagesize() * list_to_integer(lists:nth(24, Stat)));
%% collect_metrics(process_heap_bytes, Stat) ->
%%   ok.
collect_metrics(process_cpu_seconds_total, Stat) ->
  Ticks = sc_clk_tck(),
  Utime = list_to_integer(lists:nth(14, Stat)) / Ticks,
  Stime = list_to_integer(lists:nth(15, Stat)) / Ticks,
  [counter_metric([{kind, utime}], Utime),
   counter_metric([{kind, stime}], Stime)].

%%====================================================================
%% Private Parts
%%====================================================================

current_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega*1000000 + Sec.

start_time() ->
  Stat = read_stat(),
  round(list_to_integer(lists:nth(22, Stat)) / sc_clk_tck()) + get_btime().

get_btime () ->
  Content = read_proc_file("/proc/stat"),
  {match, [{Start, Length}]} = re:run(Content, "btime [\\d]+"),
  list_to_integer(string:sub_string(Content, Start + 7, Start + Length)).

open_fds_count() ->
  files_count("/proc/self/fd").

max_fds_count() ->
  Content = read_proc_file("/proc/self/limits"),
  R = re:run(Content, "Max open files[\\s]+[\\d]+"),
  {match, [{Start, Length}]} = R,
  list_to_integer(lists:nth(4, string:tokens(string:sub_string(Content, Start, Start + Length), " "))).

read_stat() ->
  Content = read_proc_file("/proc/self/stat"),
  string:tokens(Content, " ").

read_proc_file(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try read_chunk(Device)
  after file:close(Device)
  end.

read_chunk(Device) ->
  case io:get_line(Device, ?CHUNK_SIZE) of
    eof  -> [];
    Content -> Content ++ read_chunk(Device)
  end.

create_gauge(Name, Help, Data) ->
  create_mf(Name, Help, gauge, ?MODULE, Data).

create_counter(Name, Help, Data) ->
  create_mf(Name, Help, counter, ?MODULE, Data).

sc_clk_tck() ->
  not_loaded(?LINE).

sc_pagesize() ->
  not_loaded(?LINE).

files_count(_Path) ->
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
