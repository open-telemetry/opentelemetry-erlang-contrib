#pragma once

#include <stdexcept>

namespace Prometheus
{
class ProcessInfoException : public std::runtime_error
{
public:
  ProcessInfoException() : std::runtime_error("ProcessInfoException") {}
};
}