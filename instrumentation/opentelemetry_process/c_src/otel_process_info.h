#pragma once

#include <cstdint>
#include <cstdio>
#include <ctime>
#include <unistd.h>

#if defined(__FreeBSD__) || defined(__APPLE__)
#include <sys/sysctl.h>
#include <sys/user.h>
#endif

#ifdef __linux__
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>
#endif

#ifdef __APPLE__
#include <libproc.h>
#include <sys/proc_info.h>
#endif

#include "otel_exceptions.h"

#define UNUSED(x) (void)(x)

namespace Prometheus
{
    class ProcessInfo
    {
    private:
        static struct rlimit get_process_limit(int resource)
        {
            struct rlimit rlp;
            if (getrlimit(resource, &rlp))
            {
                throw ProcessInfoException();
            }

            return rlp;
        };

        void set_rusage()
        {
            struct rusage rusage;
            getrusage(RUSAGE_SELF, &rusage);
            utime_seconds = rusage.ru_utime.tv_sec + rusage.ru_utime.tv_usec / 1000000.00;
            stime_seconds = rusage.ru_stime.tv_sec + rusage.ru_stime.tv_usec / 1000000.00;
            max_rm_bytes = rusage.ru_maxrss * 1024;
            noio_pagefaults_total = rusage.ru_minflt;
            io_pagefaults_total = rusage.ru_majflt;
            swaps_total = rusage.ru_nswap;
            disk_reads_total = rusage.ru_inblock;
            disk_writes_total = rusage.ru_oublock;
            signals_delivered_total = rusage.ru_nsignals;
            voluntary_context_switches_total = rusage.ru_nvcsw;
            involuntary_context_switches_total = rusage.ru_nivcsw;
        };

        void set_fds_limit()
        {
            const auto &fds_rlimit = get_process_limit(RLIMIT_NOFILE);
            fds_limit = fds_rlimit.rlim_cur;
        };

        int get_fds_total();
        void set_proc_stat();
    public:
        pid_t pid;
        time_t now;
        int fds_total;
        uintmax_t fds_limit;
        uintmax_t start_time_seconds;
        long uptime_seconds;
        int threads_total;
        unsigned long vm_bytes;
        unsigned long rm_bytes;
        double utime_seconds;
        double stime_seconds;
        long max_rm_bytes;
        long noio_pagefaults_total;
        long io_pagefaults_total;
        long swaps_total;
        long disk_reads_total;
        long disk_writes_total;
        long signals_delivered_total;
        long voluntary_context_switches_total;
        long involuntary_context_switches_total;

        ProcessInfo()
        {
            pid = getpid();

            fds_total = get_fds_total();
            set_fds_limit();
            set_rusage();
            std::time(&now);

            set_proc_stat();
        }
    };
}
