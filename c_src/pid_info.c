#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <sys/types.h>
#include <libutil.h>

int get_process_limit(pid_t pid, int resource, struct rlimit* rlp)
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

/*
  from https://github.com/freebsd/freebsd/blob/9e0a154b0fd5fa9010238ac9497ec59f84167c92/lib/libutil/kinfo_getfile.c#L22-L51
  I don't need unpacked structs here, just count. Hope it won't break someday.
*/
int get_process_open_fds(pid_t pid, int* count)
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

int real_main(int argc, char **argv)
{
  pid_t pid;
  char *dummy; // no need to free, just a pointer

  if(argc != 2) {
    printf("Usage: %s <pid>\n", argv[0]);
    return 1;
  }

  pid = strtol(argv[1], &dummy, 10);

  if (*dummy != '\0') {
    printf("Can't parse pid: '%s'\n", argv[1]);
    return 1;
  }

  printf("Pid(%d) info:\n", pid);

  struct kinfo_proc *proc = kinfo_getproc(pid); // free at the end
  if (proc) {

    printf("Process name: %s\n", proc->ki_comm);

    int fds;
    if(get_process_open_fds(pid, &fds)) {
      printf("Process open files: n/a\n");
    } else {
      printf("Process open files: %d\n", fds);
    }

    struct rlimit rlimit; // no need to free
    if(get_process_limit(pid, RLIMIT_NOFILE, &rlimit)) {
      printf("Process fd limit: n/a\n");
    } else {
      printf("Process fd limit: %ld\n", rlimit.rlim_cur);
    }

    printf("Process start time: %zd\n", proc->ki_start.tv_sec);

    time_t now; // no need to free
    time(&now);
    printf("Process uptime: %ld\n", now - proc->ki_start.tv_sec);

    printf("Process threads count: %d\n", proc->ki_numthreads);

    printf("Process virtual memory: %ld\n", proc->ki_size);


    int pageSize;
    size_t len = sizeof(pageSize);
    if (sysctlbyname("vm.stats.vm.v_page_size", &pageSize, &len, NULL, 0) == -1) {
      pageSize = sysconf(_SC_PAGESIZE);
    }
    printf("Process resident memory: %ld\n", proc->ki_rssize * pageSize);

    struct rusage rusage = proc->ki_rusage;
    printf("Process utime: %ld\n", rusage.ru_utime.tv_sec);
    printf("Process stime: %ld\n", rusage.ru_stime.tv_sec);
    printf("Process runtime: %ld\n", proc->ki_runtime / 1000000);
    
    free(proc);
  } else {
    printf("Unknown pid\n");
  }

  return 0;
}

// limits, procstat
int main(int argc, char **argv)
{
  while(1) {
    real_main(argc, argv);
  }
  return real_main(argc, argv);
}
