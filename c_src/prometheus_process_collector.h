#ifndef PROMETHEUS_PROCESS_COLLECTOR_BACKEND_H
#define PROMETHEUS_PROCESS_COLLECTOR_BACKEND_H

#include <stdlib.h>
#ifdef __FreeBSD__
#include <sys/types.h>
#endif

struct prometheus_process_info
{
  int pids_count;
  int pids_limit;
  long start_time_seconds;
  long uptime_seconds;
  int threads_count;
  unsigned long vm_bytes;
  unsigned long rm_bytes;
  long utime_seconds;
  long stime_seconds;
};

#define PROCESS_INFO_COUNT 9

int fill_prometheus_process_info(pid_t pid, struct prometheus_process_info* prometheus_process_info);

#endif
