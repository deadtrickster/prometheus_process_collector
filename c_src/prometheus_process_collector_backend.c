#define _GNU_SOURCE

#include "prometheus_process_collector.h"

static long pagesize(void)
{
#ifdef __linux__
  return sysconf(_SC_CLK_TCK);
#endif

#ifdef __FreeBSD__
  int pageSize;
  size_t len = sizeof(pageSize);
  if (sysctlbyname("vm.stats.vm.v_page_size", &pageSize, &len, NULL, 0) == -1) {
    pageSize = sysconf(_SC_PAGESIZE);
  }
  return pageSize;
#endif
}

#ifdef __FreeBSD__

/*
  from https://github.com/freebsd/freebsd/blob/9e0a154b0fd5fa9010238ac9497ec59f84167c92/lib/libutil/kinfo_getfile.c#L22-L51
  I don't need unpacked structs here, just count. Hope it won't break someday.
*/
static int get_process_open_fd_counts(pid_t pid, int* count)
{
  int mib[4];
  int error;
  int cnt;
  size_t len;
  char *buf, *eb;
  struct kinfo_file *kf;

  // get size of all pids
  len = 0;
  mib[0] = CTL_KERN;
  mib[1] = KERN_PROC;
  mib[2] = KERN_PROC_FILEDESC;
  mib[3] = pid;

  error = sysctl(mib, nitems(mib), NULL, &len, NULL, 0);
  if (error)
    return 1;

  // allocate buf for pids
  len = len * 4 / 3;
  buf = malloc(len);
  if (buf == NULL)
    return 1;

  // fill buf with kinfo_files
  error = sysctl(mib, nitems(mib), buf, &len, NULL, 0);
  if (error) {
    free(buf);
    return 1;
  }

  // count structs in the buf
  cnt = 0;
  eb = buf + len;
  while (buf < eb) {
    kf = (struct kinfo_file *)(uintptr_t)buf;
    if (kf->kf_structsize == 0)
      break;
    buf += kf->kf_structsize;
    cnt++;
  }

  free(buf-len);
  *count = cnt;

  return 0;
}

#endif

#ifdef __linux__
static int get_process_open_fd_counts(pid_t pid, int* count)
{
  static char fd_path[32];

  sprintf(fd_path, "/proc/%d/fd", pid);

  int file_count = 0;
  DIR* dirp;
  struct dirent* entry;
  dirp = opendir(fd_path);
  if (dirp == NULL) {
    return 1;
  }
  while ((entry = readdir(dirp)) != NULL) {
    if (entry->d_type == DT_LNK) {
      file_count++;
    }
  }
  closedir(dirp);
  *count = file_count;
  return 0;
}

#endif

#ifdef __FreeBSD__
static int get_process_limit(pid_t pid, int resource, struct rlimit* rlp)
{
  int name[5];
  size_t len;

  name[0] = CTL_KERN;
  name[1] = KERN_PROC;
  name[2] = KERN_PROC_RLIMIT;
  name[3] = pid;
  name[4] = resource;
  len = sizeof(*rlp);
  return sysctl(name, 5, rlp, &len, NULL, 0);
}
#endif

#ifdef __linux__
static int get_process_limit(pid_t pid, int resource, struct rlimit* rlp)
{
  UNUSED(pid);
  return getrlimit(resource, rlp);
}
#endif

#ifdef __linux__

static int clk_tck(long *clk_tck)
{
  long result = sysconf(_SC_CLK_TCK);

  if(result == -1 || result == 0){
    return 1;
  }

  *clk_tck = result;
  return 0;
}

static int system_boot_time(long *boot_time)
{
  FILE *fd;
  char *stat_line = NULL;
  size_t len = 0;
  ssize_t read;

  fd = fopen("/proc/stat", "r");
  if (fd == NULL) {
    return 1;
  }

  while ((read = getline(&stat_line, &len, fd)) != -1 &&
         !sscanf(stat_line, "btime %ld", boot_time)) {
  }

  fclose(fd);
  if (stat_line)
    free(stat_line);


  return 0;
}

#define MAX_STAT 24

static unsigned long get_process_stat(int index, char *stat[]) {
  char *dummy;
  return strtoul(stat[index], &dummy, 10);
}

static struct kinfo_proc* kinfo_getproc(pid_t pid)
{
  long ticks;
  if(clk_tck(&ticks)) {
    return NULL;
  }

  long boot_time;
  if(system_boot_time(&boot_time)) {
    return NULL;
  }
  
  static char stat_path[32];
  sprintf(stat_path, "/proc/%d/stat", pid);

  char *stat_line = NULL;
  size_t len = 0;
  FILE *fd = fopen(stat_path, "r");
  if(!fd) {
    return NULL;
  }

  if(getline(&stat_line, &len, fd) == -1) {
    if (stat_line) {
      free(stat_line);
    }
    return NULL;
  }
  fclose(fd);

  char *old_stat_line = stat_line;
  int index = 0;  
  char *stat[MAX_STAT + 1];
  
  while (index < MAX_STAT) {
    stat[index] = strsep(&stat_line, " ");
    index++;
  }

  struct kinfo_proc *proc = malloc(sizeof(struct kinfo_proc));

  struct timeval ki_start;
  ki_start.tv_sec = get_process_stat(21, stat)/ticks + boot_time;;
  proc->ki_start = ki_start;

  proc->ki_numthreads = get_process_stat(19, stat);

  proc->ki_size = get_process_stat(22, stat);
  
  proc->ki_rssize = get_process_stat(23, stat);

  struct rusage ki_rusage;
  ki_rusage.ru_utime.tv_sec = get_process_stat(13, stat)/ticks;
  ki_rusage.ru_stime.tv_sec = get_process_stat(14, stat)/ticks;
  proc->ki_rusage = ki_rusage;

  free(old_stat_line);
  return proc;
}
#endif

int fill_prometheus_process_info(pid_t pid, struct prometheus_process_info* prometheus_process_info)
{
  struct kinfo_proc *proc = kinfo_getproc(pid);
  //printf("proc alloced at %p\r\n", proc);

  if(!proc) {
    return 1;
  }

  int pids_count;
  if(get_process_open_fd_counts(pid, &pids_count)) {
    return 1;
  }
  prometheus_process_info->pids_count = pids_count;

  struct rlimit rlimit;
  if(get_process_limit(pid, RLIMIT_NOFILE, &rlimit)) {
    return 1;
  } else {
    prometheus_process_info->pids_limit = rlimit.rlim_cur;
  }

  prometheus_process_info->start_time_seconds = proc->ki_start.tv_sec;

  time_t now;
  time(&now);
  prometheus_process_info->uptime_seconds = now - proc->ki_start.tv_sec;

  prometheus_process_info->threads_count = proc->ki_numthreads;

  prometheus_process_info->vm_bytes = proc->ki_size;

  prometheus_process_info->rm_bytes = proc->ki_rssize * pagesize();

  struct rusage rusage = proc->ki_rusage;
  prometheus_process_info->utime_seconds = rusage.ru_utime.tv_sec;
  prometheus_process_info->stime_seconds = rusage.ru_stime.tv_sec;

  free(proc);

  return 0;
}

int main(int argc, char** argv) {
  while(1) {
    struct prometheus_process_info* prometheus_process_info = malloc(sizeof(struct prometheus_process_info));
    // printf("prometheus_process_info alloced at %p\r\n", prometheus_process_info);
    fill_prometheus_process_info(36762, prometheus_process_info);    
    free(prometheus_process_info);
  }

  return 0;
}
