#include "erl_nif.h"
#include <unistd.h>

#include "otel_process_metrics.h"
#include "otel_process_info.h"

namespace Prometheus
{

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
    static ERL_NIF_TERM ATOM_PROCESS_MAX_RESIDENT_MEMORY_BYTES;
    static ERL_NIF_TERM ATOM_PROCESS_NOIO_PAGEFAULTS_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_IO_PAGEFAULTS_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_SWAPS_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_DISK_READS_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_DISK_WRITES_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_SIGNALS_DELIVERED_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_VOLUNTARY_CONTEXT_SWITCHES_TOTAL;
    static ERL_NIF_TERM ATOM_PROCESS_INVOLUNTARY_CONTEXT_SWITCHES_TOTAL;

    static ERL_NIF_TERM process_info_plist[PROCESS_INFO_COUNT];

/* ERL_NIF_TERM */
/* mk_error(ErlNifEnv* env, const char* mesg) */
/* { */
/*   return enif_make_tuple2(env, ATOM_ERROR, mk_atom(env, mesg)); */
/* } */

    static ERL_NIF_TERM get_process_info(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
    {
        ProcessInfo process_info;

        process_info_plist[0] = enif_make_tuple2(env, ATOM_PROCESS_OPEN_FDS, enif_make_int(env, process_info.fds_total));
        process_info_plist[1] = enif_make_tuple2(env, ATOM_PROCESS_MAX_FDS, enif_make_int(env, process_info.fds_limit));
        process_info_plist[2] = enif_make_tuple2(env, ATOM_PROCESS_START_TIME_SECONDS, enif_make_long(env, process_info.start_time_seconds));
        process_info_plist[3] = enif_make_tuple2(env, ATOM_PROCESS_UPTIME_SECONDS, enif_make_long(env, process_info.uptime_seconds));
        process_info_plist[4] = enif_make_tuple2(env, ATOM_PROCESS_THREADS_TOTAL, enif_make_int(env, process_info.threads_total));
        process_info_plist[5] = enif_make_tuple2(env, ATOM_PROCESS_VIRTUAL_MEMORY_BYTES, enif_make_ulong(env, process_info.vm_bytes));
        process_info_plist[6] = enif_make_tuple2(env, ATOM_PROCESS_RESIDENT_MEMORY_BYTES, enif_make_ulong(env, process_info.rm_bytes));
        process_info_plist[7] = enif_make_tuple2(env, ATOM_PROCESS_UTIME_SECONDS, enif_make_double(env, process_info.utime_seconds));
        process_info_plist[8] = enif_make_tuple2(env, ATOM_PROCESS_STIME_SECONDS, enif_make_double(env, process_info.stime_seconds));
        process_info_plist[9] = enif_make_tuple2(env, ATOM_PROCESS_MAX_RESIDENT_MEMORY_BYTES, enif_make_long(env, process_info.max_rm_bytes));
        process_info_plist[10] = enif_make_tuple2(env, ATOM_PROCESS_NOIO_PAGEFAULTS_TOTAL, enif_make_long(env, process_info.noio_pagefaults_total));
        process_info_plist[11] = enif_make_tuple2(env, ATOM_PROCESS_IO_PAGEFAULTS_TOTAL, enif_make_long(env, process_info.io_pagefaults_total));
        process_info_plist[12] = enif_make_tuple2(env, ATOM_PROCESS_SWAPS_TOTAL, enif_make_long(env, process_info.swaps_total));
        process_info_plist[13] = enif_make_tuple2(env, ATOM_PROCESS_DISK_READS_TOTAL, enif_make_long(env, process_info.disk_reads_total));
        process_info_plist[14] = enif_make_tuple2(env, ATOM_PROCESS_DISK_WRITES_TOTAL, enif_make_long(env, process_info.disk_writes_total));
        process_info_plist[15] = enif_make_tuple2(env, ATOM_PROCESS_SIGNALS_DELIVERED_TOTAL, enif_make_long(env, process_info.signals_delivered_total));
        process_info_plist[16] = enif_make_tuple2(env, ATOM_PROCESS_VOLUNTARY_CONTEXT_SWITCHES_TOTAL, enif_make_long(env, process_info.voluntary_context_switches_total));
        process_info_plist[17] = enif_make_tuple2(env, ATOM_PROCESS_INVOLUNTARY_CONTEXT_SWITCHES_TOTAL, enif_make_long(env, process_info.involuntary_context_switches_total));

        return enif_make_list_from_array(env, process_info_plist, 18);
    }
}

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{

#define ATOM(Id, Value)                         \
    {                                           \
        Id = enif_make_atom(env, Value);        \
    }
    // initialize the atoms
    using namespace Prometheus;

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
    ATOM(ATOM_PROCESS_MAX_RESIDENT_MEMORY_BYTES, "process_max_resident_memory_bytes");
    ATOM(ATOM_PROCESS_NOIO_PAGEFAULTS_TOTAL, "process_noio_pagefaults_total");
    ATOM(ATOM_PROCESS_IO_PAGEFAULTS_TOTAL, "process_io_pagefaults_total");
    ATOM(ATOM_PROCESS_SWAPS_TOTAL, "process_swaps_total");
    ATOM(ATOM_PROCESS_DISK_READS_TOTAL, "process_disk_reads_total");
    ATOM(ATOM_PROCESS_DISK_WRITES_TOTAL, "process_disk_writes_total");
    ATOM(ATOM_PROCESS_SIGNALS_DELIVERED_TOTAL, "process_signals_delivered_total");
    ATOM(ATOM_PROCESS_VOLUNTARY_CONTEXT_SWITCHES_TOTAL, "process_voluntary_context_switches_total");
    ATOM(ATOM_PROCESS_INVOLUNTARY_CONTEXT_SWITCHES_TOTAL, "process_involuntary_context_switches_total");
#undef ATOM

    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"get_process_info", 0, Prometheus::get_process_info}};

extern "C" {
    ERL_NIF_INIT(opentelemetry_process_metrics, nif_funcs, &on_load, NULL, NULL, NULL);
}
