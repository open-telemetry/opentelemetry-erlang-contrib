#pragma once

#include <stdio.h>
#include <stdlib.h>
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

#define UNUSED(x) (void)(x)

struct otel_process_info
{
  int fds_total;
  rlim_t fds_limit;
  time_t start_time_seconds;
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
};

#ifdef __linux__

struct kinfo_proc
{
  struct timeval ki_start; /* starting time */
  int ki_numthreads;       /* XXXKSE number of threads in total */
  unsigned long ki_size;   /* virtual size */
  unsigned long ki_rssize; /* resident size in pages */
  struct rusage ki_rusage; /* process rusage statistics */
};

#endif

#define PROCESS_INFO_COUNT 18

int fill_otel_process_info(pid_t pid, struct otel_process_info *otel_process_info);