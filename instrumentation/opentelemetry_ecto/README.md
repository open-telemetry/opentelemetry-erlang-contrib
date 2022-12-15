# OpentelemetryEcto

Telemetry handler that creates Opentelemetry spans from Ecto query events. Because
Ecto emits telemetry events only after queries have finished, OpentelemetryEcto
estimates the start time of the span by subtracting the reported total duration
from the current timestamp.

After installing, setup the handler in your application behaviour before your
top-level supervisor starts.

```elixir
OpentelemetryEcto.setup([:blog, :repo])
```

See the documentation for `OpentelemetryEcto.setup/2` for additional options that
may be supplied.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_ecto` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_ecto, "~> 1.0"}
  ]
end
```

## Compatibility Matrix

| OpentelemetryEcto Version | Otel Version | Notes |
| :------------------------ | :----------- | :---- |
|                           |              |       |
| v0.1.0                    | <= v.0.5.0   |       |
| v1.0.0-rc.1               | v1.0.0-rc.1  |       |
| v1.0.0-rc.2               | v1.0.0-rc.2  |       |
| v1.0.0-rc.3               | v1.0.0-rc.3  |       |
| v1.0.0-rc.4               | v1.0.0-rc.4  |       |
| v1.0                      | v1.0         |       |

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_ecto](https://hexdocs.pm/opentelemetry_ecto).

