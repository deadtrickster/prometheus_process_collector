#define _GNU_SOURCE
#include "erl_nif.h"
#include <dirent.h>
#include <unistd.h>

#include "prometheus_process_collector.h"

#define MAXBUFLEN 1024

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_PROCESS_OPEN_FDS;
static ERL_NIF_TERM ATOM_PROCESS_MAX_FDS;
static ERL_NIF_TERM ATOM_PROCESS_START_TIME_SECONDS;
static ERL_NIF_TERM ATOM_PROCESS_UPTIME_SECONDS;
static ERL_NIF_TERM ATOM_PROCESS_THREADS_TOTAL;
static ERL_NIF_TERM ATOM_PROCESS_VIRTUAL_MEMORY_BYTES;
static ERL_NIF_TERM ATOM_PROCESS_RESIDENT_MEMORY_BYTES;
static ERL_NIF_TERM ATOM_PROCESS_UTIME_SECONDS;
static ERL_NIF_TERM ATOM_PROCESS_STIME_SECONDS;

static ERL_NIF_TERM process_info_plist[PROCESS_INFO_COUNT];

/* ERL_NIF_TERM */
/* mk_error(ErlNifEnv* env, const char* mesg) */
/* { */
/*   return enif_make_tuple2(env, ATOM_ERROR, mk_atom(env, mesg)); */
/* } */

static ERL_NIF_TERM get_process_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  pid_t pid;
  if(argc == 1) {
    if (!enif_get_int(env, argv[0], &pid))
      return enif_make_badarg(env);
  } else {
    pid = getpid();
  }

  struct prometheus_process_info* prometheus_process_info = (struct prometheus_process_info*)malloc(sizeof(prometheus_process_info));

  if(fill_prometheus_process_info(pid, prometheus_process_info)) {
    return ATOM_ERROR;
  }

  process_info_plist[0] = enif_make_tuple2(env, ATOM_PROCESS_OPEN_FDS, enif_make_int(env, prometheus_process_info->pids_count));
  process_info_plist[1] = enif_make_tuple2(env, ATOM_PROCESS_MAX_FDS, enif_make_int(env, prometheus_process_info->pids_limit));
  process_info_plist[2] = enif_make_tuple2(env, ATOM_PROCESS_START_TIME_SECONDS, enif_make_long(env, prometheus_process_info->start_time_seconds));
  process_info_plist[3] = enif_make_tuple2(env, ATOM_PROCESS_UPTIME_SECONDS, enif_make_long(env, prometheus_process_info->uptime_seconds));
  process_info_plist[4] = enif_make_tuple2(env, ATOM_PROCESS_THREADS_TOTAL, enif_make_int(env, prometheus_process_info->threads_count));
  process_info_plist[5] = enif_make_tuple2(env, ATOM_PROCESS_VIRTUAL_MEMORY_BYTES, enif_make_ulong(env, prometheus_process_info->vm_bytes));
  process_info_plist[6] = enif_make_tuple2(env, ATOM_PROCESS_RESIDENT_MEMORY_BYTES, enif_make_ulong(env, prometheus_process_info->rm_bytes));
  process_info_plist[7] = enif_make_tuple2(env, ATOM_PROCESS_UTIME_SECONDS, enif_make_long(env, prometheus_process_info->utime_seconds));
  process_info_plist[8] = enif_make_tuple2(env, ATOM_PROCESS_STIME_SECONDS, enif_make_long(env, prometheus_process_info->stime_seconds));

  return enif_make_list_from_array(env, process_info_plist, 9);
}


static ErlNifFunc nif_funcs[] =
  {
   {"get_process_info", 0, get_process_info}/* , */
   /* {"get_process_info", 1, get_process_info} */
  };

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
  // initialize the atoms
  ATOM(ATOM_OK, "ok");
  ATOM(ATOM_ERROR, "error");
  ATOM(ATOM_PROCESS_OPEN_FDS, "process_open_fds");
  ATOM(ATOM_PROCESS_MAX_FDS, "process_max_fds");
  ATOM(ATOM_PROCESS_START_TIME_SECONDS, "process_start_time_seconds");
  ATOM(ATOM_PROCESS_UPTIME_SECONDS, "process_uptime_seconds");
  ATOM(ATOM_PROCESS_THREADS_TOTAL, "process_threads_total");
  ATOM(ATOM_PROCESS_VIRTUAL_MEMORY_BYTES, "process_virtual_memory_bytes");
  ATOM(ATOM_PROCESS_RESIDENT_MEMORY_BYTES, "process_resident_memory_bytes");
  ATOM(ATOM_PROCESS_UTIME_SECONDS, "process_utime_seconds");
  ATOM(ATOM_PROCESS_STIME_SECONDS, "process_stime_seconds");
#undef ATOM

  return 0;

}

ERL_NIF_INIT(prometheus_process_collector, nif_funcs, &on_load, NULL, NULL, NULL);
