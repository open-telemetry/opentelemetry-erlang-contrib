# OpentelemetryFinch

OpentelemetryFinch uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
create `OpenTelemetry` spans from Finch events.

## Installation

The package can be installed by adding `opentelemetry_finch` to your list of
dependencies in `mix.exs`:

```elixir
  def deps do
    [
      {:opentelemetry_finch, "~> 0.1"}
    ]
  end
```

In your application start:

```elixir
  def start(_type, _args) do
    OpentelemetryFinch.setup()

    # ...
  end
```

## Compatibility Matrix

| OpentelemetryFinch Version | Otel Version | Notes |
| :------------------------- | :----------- | :---- |
|                            |              |       |
| v0.1.0                     | v1.0.0       |       |


Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_finch](https://hexdocs.pm/opentelemetry_finch).

