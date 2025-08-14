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

For Broadway pipelines that need distributed tracing context extraction from message headers:

```elixir
def start(_type, _args) do
  # Enable trace propagation from message headers
  OpentelemetryBroadway.setup(propagation: true)

  Supervisor.start_link(...)
end
```

**Important**: When using trace propagation, your producer must be configured to extract headers. For RabbitMQ, your `BroadwayRabbitMQ.Producer` must be configured with `metadata: [:headers]` to extract trace context from message headers:

```elixir
Broadway.start_link(MyBroadway,
  name: MyBroadway,
  producer: [
    module: {BroadwayRabbitMQ.Producer,
      queue: "my_queue",
      metadata: [:headers],  # Required for trace propagation!
      connection: [
        username: "user",
        password: "password",
        host: "localhost"
      ]
    }
  ],
  processors: [default: [concurrency: 10]]
)
```

## Features

### Basic Instrumentation

- Creates spans for Broadway message processing
- Tracks message processing duration
- Records exceptions and failures
- Adds semantic attributes for messaging operations

### Enhanced Instrumentation (with propagation)

All basic features plus:

- **Automatic trace propagation**: Extracts distributed tracing context from message headers
- **Parent trace linking**: Links to upstream traces when trace context is found
- **Standards compliance**: Supports W3C Trace Context and other OpenTelemetry propagation formats

### Semantic Attributes

The instrumentation adds the following OpenTelemetry semantic attributes:

#### Basic Attributes (all modes)

- `messaging.system`: Set to `"broadway"`
- `messaging.operation`: Set to `"process"`
- `messaging.consumer.id`: Broadway consumer identifier
- `messaging.message.payload_size_bytes`: Message payload size (if available)

## Installation

This library is available on Hex:

```elixir
defp deps do
  [
    {:opentelemetry_broadway, "~> 0.1"}
  ]
end
```

## Credit

This repository was [originally created](https://github.com/breakroom/opentelemetry_broadway) by [Tom Taylor](https://github.com/tomtaylor).
