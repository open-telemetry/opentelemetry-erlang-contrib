#include <stdlib.h>
#include <iostream>
#include <memory>

#include "otel_process_info.h"

int main(int argc, char **argv)
{

  if (argc != 2)
  {
    // printf("Usage: %s <iterations>\n", argv[0]);
    return 1;
  }

  char *dummy;
  unsigned long iterations = strtoul(argv[1], &dummy, 10);

  if (*dummy != '\0')
  {
    // printf("Iterations must be an integer\n");
    return 1;
  };

  while (iterations--)
  {
      auto process_info = std::unique_ptr<Prometheus::ProcessInfo>(new Prometheus::ProcessInfo());
      std::cout << process_info->threads_total << std::endl;
  }

  return 0;
}
