# OpentelemetryTelemetry

A utility library for creating OpenTelemetry spans from telemetry events.

## Purpose

Most libraries in the BEAM ecosystem leverage [telemetry](https://github.com/beam-telemetry/telemetry) events for exposing
event hook points for monitoring that library. While OpenTelemetry is a great project,
it is still one specification for monitoring software and it isn't
reasonable to ask library authors to support multiple conventions.

OpentelemetryTelemetry provides mechanisms for otel instrumentation libraries
to leverage telemetry events for creating and managing spans. The instrumentation library
is then able to leverage the telemetry measurements and metadata for deriving
spans, adding attributes, set span names, etc.

### What Opentelemetry is Not

This library is only intended to provide utilities for working with telemetry
events to instrumentation libraries. As such, it should not be used directly
within your application code where the OpenTelemery API library should be leveraged.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_telemetry` to your list of dependencies:

```erlang
{deps, [
  {opentelemetry_telemetry, "~> 1.0"}
]}.
```

```elixir
def deps do
  [
    {:opentelemetry_telemetry, "~> 1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_telemetry](https://hexdocs.pm/opentelemetry_telemetry).

