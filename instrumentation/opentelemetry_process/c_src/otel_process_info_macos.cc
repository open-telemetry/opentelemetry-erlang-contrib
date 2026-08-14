#include "otel_process_info.h"
#include <cstdio>
#include <ctime>
#include <memory>

namespace Prometheus
{
    static std::unique_ptr<kinfo_proc> kinfo_getproc(pid_t pid)
    {
        int mib[4];
        size_t len;

        len = 0;
        mib[0] = CTL_KERN;
        mib[1] = KERN_PROC;
        mib[2] = KERN_PROC_PID;
        mib[3] = pid;
        if (sysctl(mib, 4, NULL, &len, NULL, 0) < 0)
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

        return kipp;
    }

    static struct proc_taskinfo proc_pidtaskinfo(pid_t pid)
    {
        struct proc_taskinfo pti;
        if (PROC_PIDTASKINFO_SIZE == proc_pidinfo(pid, PROC_PIDTASKINFO, 0, &pti, PROC_PIDTASKINFO_SIZE))
        {
            return pti;
        }
        else
        {
            throw ProcessInfoException();
        }
    }
    
    int ProcessInfo::get_fds_total()
    {
        int total_size = proc_pidinfo(pid, PROC_PIDLISTFDS, 0, 0, 0);
        if (total_size < 0)
        {
            throw ProcessInfoException();
        }

        return total_size / PROC_PIDLISTFD_SIZE;
    }

    void ProcessInfo::set_proc_stat()
    {        
        auto proc = kinfo_getproc(pid);
        const auto &pti = proc_pidtaskinfo(pid);
        
        start_time_seconds = proc->kp_proc.p_starttime.tv_sec;
        uptime_seconds = now - start_time_seconds;
        threads_total = pti.pti_threadnum;
        vm_bytes = pti.pti_virtual_size;
        rm_bytes = pti.pti_resident_size;
    }
}
