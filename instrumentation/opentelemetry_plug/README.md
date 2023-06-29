# OpentelemetryPlug

Telemetry handler that creates Opentelemetry spans from Plug.Router events.

After installing, setup the handler in your application behaviour before your
top-level supervisor starts.

```elixir
OpentelemetryPlug.setup()
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_plug` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_plug, "~> 1.0"}
  ]
end
```

## Compatibility Matrix

| OpentelemetryPlug Version | Otel Version | Notes |
| :------------------------ | :----------- | :---- |
|                           |              |       |
| v1.0                      | v1.0         |       |

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_plug](https://hexdocs.pm/opentelemetry_plug).
