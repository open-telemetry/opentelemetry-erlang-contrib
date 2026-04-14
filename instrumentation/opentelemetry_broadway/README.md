# opentelemetry_broadway

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_cowboy)](https://hex.pm/packages/opentelemetry_cowboy)
![Build Status](https://github.com/open-telemetry/opentelemetry-erlang-contrib/workflows/Erlang/badge.svg)

OpenTelemetry tracing for [Broadway](https://elixir-broadway.org/) pipelines with support for distributed tracing.

## Usage

### Basic Setup

For basic Broadway instrumentation, set up the handler in your application's `Application.start/2` callback:

```elixir
def start(_type, _args) do
  OpentelemetryBroadway.setup()

  Supervisor.start_link(...)
end
```

### With Trace Propagation

For Broadway pipelines that need distributed tracing with linked spans across services (extracts context from message headers and creates trace links):

```elixir
def start(_type, _args) do
  # Enable trace propagation from message headers
  OpentelemetryBroadway.setup(propagation: true)

  Supervisor.start_link(...)
end
```

## Installation

This library is available on Hex:

```elixir
defp deps do
  [
    {:opentelemetry_broadway, "~> 0.3"}
  ]
end
```

## Credit

This repository was [originally created](https://github.com/breakroom/opentelemetry_broadway) by [Tom Taylor](https://github.com/tomtaylor).
