Prometheus.io Process Collector [![Hex.pm](https://img.shields.io/hexpm/v//prometheus_process_collector.svg?maxAge=2592000)](https://hex.pm/packages/prometheus_process_collector)
=====

```
# TYPE process_open_fds gauge
# HELP process_open_fds Number of open file descriptors.
process_open_fds 11
# TYPE process_max_fds gauge
# HELP process_max_fds Maximum number of open file descriptors.
process_max_fds 20000
# TYPE process_start_time_seconds gauge
# HELP process_start_time_seconds Start time of the process since unix epoch in seconds.
process_start_time_seconds 1469491524
# TYPE process_uptime_seconds gauge
# HELP process_uptime_seconds Process uptime in seconds.
process_uptime_seconds 18
# TYPE process_threads_total gauge
# HELP process_threads_total Process Threads count.
process_threads_total 18
# TYPE process_virtual_memory_bytes gauge
# HELP process_virtual_memory_bytes Virtual memory size in bytes.
process_virtual_memory_bytes 2785275904
# TYPE process_resident_memory_bytes gauge
# HELP process_resident_memory_bytes Resident memory size in bytes.
process_resident_memory_bytes 57053184
# TYPE process_cpu_seconds_total counter
# HELP process_cpu_seconds_total Process CPU seconds total.
process_cpu_seconds_total{kind="utime"} 2.46
process_cpu_seconds_total{kind="stime"} 5.44
```

Build
-----

    $ rebar3 compile
