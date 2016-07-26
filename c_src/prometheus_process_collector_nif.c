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
