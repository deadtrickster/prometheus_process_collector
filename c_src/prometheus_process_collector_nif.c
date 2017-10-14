#define _GNU_SOURCE
#include "erl_nif.h"
#include <dirent.h>
#include <unistd.h>

ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);

#define MAXBUFLEN 1024

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM ret;

  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
      return enif_make_atom(env, atom);
    }

  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
sc_clk_tck(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 0)
    {
      return enif_make_badarg(env);
    }

  long result = sysconf(_SC_CLK_TCK);

  if(result == -1 || result == 0)
    {
      return mk_error(env, "sysconf_fail");
    }

  return enif_make_ulong(env, result);
}

static ERL_NIF_TERM
sc_pagesize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 0)
    {
      return enif_make_badarg(env);
    }

  long result = sysconf(_SC_PAGESIZE);

  if(result == -1 || result == 0)
    {
      return mk_error(env, "sysconf_fail");
    }

  return enif_make_ulong(env, result);
}

static ERL_NIF_TERM
files_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  long file_count = 0;
  DIR* dirp;
  struct dirent* entry;
  char path[MAXBUFLEN];

  enif_get_string(env, argv[0], path, 1024, ERL_NIF_LATIN1);

  if(argc != 1)
    {
      return enif_make_badarg(env);
    }
  
  dirp = opendir(path);
  if (dirp == NULL) {
    return mk_error(env, "opendir_fail");
  }
  while ((entry = readdir(dirp)) != NULL) {
    if (entry->d_type == DT_LNK) {
      file_count++;
    }
  }
  closedir(dirp);
  return enif_make_ulong(env, file_count);
}

static ErlNifFunc nif_funcs[] = {
  {"sc_clk_tck", 0, sc_clk_tck},
  {"sc_pagesize", 0, sc_pagesize},
  {"files_count", 1, files_count}
};

ERL_NIF_INIT(prometheus_process_collector, nif_funcs, NULL, NULL, NULL, NULL);


/* FreeBSD
get process by pid:
      
   pid_t pid = ...;
   struct kinfo_proc *proc = kinfo_getproc(pid);
   if (proc) {
     free(proc);
   }


process_open_fds:

   int cnt;
   files = kinfo_getfile(kp->ki_pid, &cnt);
   if(files) {
     free(files)
   }

   or https://github.com/freebsd/freebsd/blob/9e0a154b0fd5fa9010238ac9497ec59f84167c92/lib/libutil/kinfo_getfile.c#L24-L40


process_start_time_seconds:
   kproc->ki_start.tv_sec


process_uptime_seconds:
   start_time_seconds - now [what about time adjustments?]


process_threads_total:
   kproc->ki_numthreads


process_virtual_memory_bytes:   
      proc->m_size = kproc->ki_size;

process_resident_memory_bytes:
      static int pageSizeKb;
      len = sizeof(pageSize);
      if (sysctlbyname("vm.stats.vm.v_page_size", &pageSize, &len, NULL, 0) == -1) {
         pageSize = PAGE_SIZE;
         pageSizeKb = PAGE_SIZE_KB;
      } else {
         pageSizeKb = pageSize / ONE_K;
      }
      proc->m_resident = kproc->ki_rssize * pageSizeKb;


process_cpu_seconds_total:
https://github.com/freebsd/freebsd/blob/4281746ee51f1fe12c328d19e730e80f1512858e/sys/sys/user.h#L202

process_fd_limit:
https://github.com/freebsd/freebsd/blob/master/usr.bin/limits/limits.c

process_threads_limit:



 */


