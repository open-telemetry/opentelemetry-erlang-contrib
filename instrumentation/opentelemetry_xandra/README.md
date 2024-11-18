# OpentelemetryXandra

This library uses [Telemetry](https://github.com/beam-telemetry/telemetry/) events to create OpenTelemetry Spans for [Xandra](https://github.com/whatyouhide/xandra) queries.

## Installation

Add `opentelemetry_xandra` to your list of dependencies in `mix.exs`:

```elixir
defp deps do
  [
    # Other opentelemetry_* deps...,
    {:opentelemetry_xandra, "~> 0.1"}
  ]
end
```

## Compatibility Matrix

| OpentelemetryXandra Version | OpenTelemetry Version |
| :-------------------------- | :-------------------- |
| v0.1.0                      | v1.0.0                |
| v0.2.0                      | v1.0.0                |
