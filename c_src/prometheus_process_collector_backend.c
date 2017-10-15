#define _GNU_SOURCE
#include <dirent.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/user.h>
#include <sys/types.h>
#include <libutil.h>

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
static int get_process_open_fds(pid_t pid, int* count)
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

int fill_prometheus_process_info(pid_t pid, struct prometheus_process_info* prometheus_process_info)
{
  struct kinfo_proc *proc = kinfo_getproc(pid);

  if(!proc) {
    return 1;
  }

  if(get_process_open_fds(pid, &prometheus_process_info->pids_count)) {
    return 1;
  }

  struct rlimit rlimit; // no need to free
  if(get_process_limit(pid, RLIMIT_NOFILE, &rlimit)) {
    return 1;
  } else {
    prometheus_process_info->pids_limit = rlimit.rlim_cur;
  }

  prometheus_process_info->start_time_seconds = proc->ki_start.tv_sec;

  time_t now; // no need to free
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

#endif

/* #ifdef __linux__ */

/* static ERL_NIF_TERM */
/* sc_clk_tck(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*   if(argc != 0) */
/*     { */
/*       return enif_make_badarg(env); */
/*     } */

/*   long result = sysconf(_SC_CLK_TCK); */

/*   if(result == -1 || result == 0) */
/*     { */
/*       return mk_error(env, "sysconf_fail"); */
/*     } */

/*   return enif_make_ulong(env, result); */
/* } */

/* static ERL_NIF_TERM */
/* files_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*   long file_count = 0; */
/*   DIR* dirp; */
/*   struct dirent* entry; */
/*   char path[MAXBUFLEN]; */

/*   enif_get_string(env, argv[0], path, 1024, ERL_NIF_LATIN1); */

/*   if(argc != 1) */
/*     { */
/*       return enif_make_badarg(env); */
/*     } */

/*   dirp = opendir(path); */
/*   if (dirp == NULL) { */
/*     return mk_error(env, "opendir_fail"); */
/*   } */
/*   while ((entry = readdir(dirp)) != NULL) { */
/*     if (entry->d_type == DT_LNK) { */
/*       file_count++; */
/*     } */
/*   } */
/*   closedir(dirp); */
/*   return enif_make_ulong(env, file_count); */
/* } */

/* #endif */

/* static ERL_NIF_TERM */
/* sc_pagesize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*   if(argc != 0) */
/*     { */
/*       return enif_make_badarg(env); */
/*     } */

/*   long result = sysconf(_SC_PAGESIZE); */

/*   if(result == -1 || result == 0) */
/*     { */
/*       return mk_error(env, "sysconf_fail"); */
/*     } */

/*   return enif_make_ulong(env, result); */
/* } */
