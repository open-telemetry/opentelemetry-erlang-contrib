#include "otel_process_info.h"
#include <cstdio>
#include <ctime>
#include <memory>

namespace Prometheus
{
    static long pagesize(void)
    {
        uint pageSize;
        size_t len = sizeof(pageSize);
        if (sysctlbyname("vm.stats.vm.v_page_size", &pageSize, &len, NULL, 0) == -1)
        {
            long spageSize = sysconf(_SC_PAGESIZE);
            if (spageSize == -1)
            {
                throw ProcessInfoException();
            }
            else
            {
                return spageSize;
            }
        }
        else
        {
            return pageSize;
        }
    }

    static std::unique_ptr<kinfo_proc> kinfo_getproc(pid_t pid)
    {
        int mib[4];
        size_t len;

        len = 0;
        mib[0] = CTL_KERN;
        mib[1] = KERN_PROC;
        mib[2] = KERN_PROC_PID;
        mib[3] = pid;
        if (sysctl(mib, nitems(mib), NULL, &len, NULL, 0) < 0)
        {
            throw ProcessInfoException();
        }
        std::unique_ptr<kinfo_proc>
            kipp {reinterpret_cast<kinfo_proc*>(new char[len])};

        if (sysctl(mib, 4, kipp.get(), &len, NULL, 0) < 0)
        {
            throw ProcessInfoException();
        }
        if (len != sizeof(*kipp))
        {
            throw ProcessInfoException();
        }
        if (kipp->ki_structsize != sizeof(*kipp))
        {
            throw ProcessInfoException();
        }
        if (kipp->ki_pid != pid)
        {
            throw ProcessInfoException();
        }

        return kipp;
    }

    /*
      from https://github.com/freebsd/freebsd/blob/9e0a154b0fd5fa9010238ac9497ec59f84167c92/lib/libutil/kinfo_getfile.c#L22-L51
      I don't need unpacked structs here, just count. Hope it won't break someday.
    */
    int ProcessInfo::get_fds_total()
    {
        int mib[4];
        int error;
        int count;
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
        {
            throw ProcessInfoException();
        }

        // allocate buf for pids
        len = len * 4 / 3;
        buf = (char*)malloc(len);
        if (buf == NULL)
        {
            throw ProcessInfoException();
        }

        // fill buf with kinfo_files
        error = sysctl(mib, nitems(mib), buf, &len, NULL, 0);
        if (error)
        {
            free(buf);
            throw ProcessInfoException();
        }

        // count structs in the buf
        count = 0;
        eb = buf + len;
        while (buf < eb)
        {
            kf = (struct kinfo_file *)(uintptr_t)buf;
            if (kf->kf_structsize == 0)
                break;
            buf += kf->kf_structsize;
            count++;
        }

        free(buf - len);
        return count;
    }

    void ProcessInfo::set_proc_stat()
    {
        auto proc = kinfo_getproc(pid);

        start_time_seconds = proc->ki_start.tv_sec;
        uptime_seconds = now - proc->ki_start.tv_sec;
        threads_total = proc->ki_numthreads;
        vm_bytes = proc->ki_size;
        rm_bytes = proc->ki_rssize * pagesize();
    }
}
