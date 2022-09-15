# OpentelemetryMonitor

This library makes it possible to monitor a process, and close its spans when it has died.

Without this, a crashed process can result in missing spans.

To use, add the process to your supervision tree:

```elixir
children = [
  OpentelemetryMonitor
]
```

Then, call `OpentelemetryMonitor.monitor(span_ctx)` as appropriate.

Example:

```elixir
OpentelemetryMonitor.monitor(OpenTelemetry.Tracer.current_span_ctx())
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_monitor` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_monitor, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/opentelemetry_monitor>.

