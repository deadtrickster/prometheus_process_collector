#ifndef PROMETHEUS_PROCESS_COLLECTOR_BACKEND_H
#define PROMETHEUS_PROCESS_COLLECTOR_BACKEND_H

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <sys/types.h>

#ifdef __FreeBSD__
#include <libutil.h>
#include <sys/types.h>
#endif

#ifdef __linux__
#include <dirent.h>
#include <string.h>
#include <time.h>
#include <sys/resource.h>
#include <errno.h>
#endif

#define UNUSED(x) (void)(x)

struct prometheus_process_info
{
  int pids_count;
  rlim_t pids_limit;
  time_t start_time_seconds;
  long uptime_seconds;
  int threads_count;
  unsigned long vm_bytes;
  unsigned long rm_bytes;
  long utime_seconds;
  long stime_seconds;
};

#ifdef __linux__

struct kinfo_proc {
  struct        timeval ki_start;       /* starting time */
  int           ki_numthreads;          /* XXXKSE number of threads in total */
  unsigned long ki_size;                /* virtual size */
  unsigned long ki_rssize;              /* resident size in pages */
  struct        rusage ki_rusage;       /* process rusage statistics */
};

#endif

#define PROCESS_INFO_COUNT 9

int fill_prometheus_process_info(pid_t pid, struct prometheus_process_info* prometheus_process_info);

#endif
