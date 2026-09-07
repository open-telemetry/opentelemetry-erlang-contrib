#include "otel_process_info.h"
#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <vector>
#include <iterator>

namespace Prometheus
{
    static long pagesize(void)
    {
        auto ret = sysconf(_SC_PAGESIZE);
        if (ret == -1)
        {
            throw ProcessInfoException();
        }
        else
        {
            return ret;
        }
    }

    static long clk_tck()
    {
        long result = sysconf(_SC_CLK_TCK);

        if (result == -1 || result == 0)
        {
            throw ProcessInfoException();
        }

        return result;
    }

    static long system_boot_time()
    {
        std::ifstream file("/proc/stat");
        std::string line;
        long boot_time;

        while (std::getline(file, line) &&
               !sscanf(line.c_str(), "btime %ld", &boot_time))
        {
        }

        return boot_time;
    }

    static std::vector<std::string> read_proc_stat(pid_t pid)
    {
        static char stat_path[32];
        std::sprintf(stat_path, "/proc/%d/stat", pid);
        std::ifstream file(stat_path);
        std::string line;
        std::getline(file, line);
        std::istringstream ils(line);
        return std::vector<std::string>{std::istream_iterator<std::string>{ils},
                                        std::istream_iterator<std::string>{}};
    }

    static unsigned long get_process_stat(int index, const std::vector<std::string> &stat)
    {
        return std::stoul(stat[index]);
    }

    int ProcessInfo::get_fds_total()
    {
        static char fd_path[32];

        std::sprintf(fd_path, "/proc/%d/fd", pid);

        DIR *dirp = opendir(fd_path);
        if (dirp == NULL)
        {
            throw ProcessInfoException();
        }

        int file_total = 0;
        struct dirent *entry;
        while ((entry = readdir(dirp)) != NULL)
        {
            if (entry->d_type == DT_LNK)
            {
                file_total++;
            }
        }

        closedir(dirp);
        return file_total;
    }

    void ProcessInfo::set_proc_stat()
    {
        long ticks = clk_tck();
        long boot_time = system_boot_time();

        const auto &stat = read_proc_stat(pid);

        start_time_seconds = get_process_stat(21, stat) / ticks + boot_time;
        uptime_seconds = now - start_time_seconds;
        threads_total = get_process_stat(19, stat);
        vm_bytes = get_process_stat(22, stat);
        rm_bytes = get_process_stat(23, stat) * pagesize();
    }
}
