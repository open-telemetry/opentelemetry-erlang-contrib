# opentelemetry_broadway

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_cowboy)](https://hex.pm/packages/opentelemetry_cowboy)
![Build Status](https://github.com/open-telemetry/opentelemetry-erlang-contrib/workflows/Erlang/badge.svg)

OpenTelemetry tracing for [Broadway](https://elixir-broadway.org/) pipelines with support for distributed tracing.
The instrumentation covers both message processing and batch processing.

## Usage

### Basic Setup

For basic Broadway instrumentation, set up the handler in your application's `Application.start/2` callback:

```elixir
def start(_type, _args) do
  OpentelemetryBroadway.setup()

  Supervisor.start_link(...)
end
```

### Trace Coverage

`opentelemetry_broadway` emits consumer spans for:

- Broadway processor message handling
- Broadway batch processor `handle_batch/4` execution

Message spans may include:

- `messaging.message.body.size`
- `messaging.message.id`

Batch spans include:

- `messaging.batch.message_count`
- `messaging.broadway.batch.successful_count`
- `messaging.broadway.batch.failed_count`

### With Trace Propagation

For Broadway pipelines that need distributed tracing with linked spans across services (extracts context from message headers and creates trace links):

```elixir
def start(_type, _args) do
  OpentelemetryBroadway.setup(span_relationship: :link)

  Supervisor.start_link(...)
end
```

Use `span_relationship: :child` when Broadway should continue an extracted parent context for
single-message processor spans. Batch spans still use links when a single parent-child
relationship would be ambiguous across multiple messages.

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
