# OpentelemetryRedix

OpentelemetryRedix uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
create `OpenTelemetry` spans from Redix command events.

Supported events include command stop. Connection and disconnection events
are also observed to track Redis instance address.

## Note on Redix integration

A sidecar process runs under `opentelemetry_redix` application to track
Redix connection information to inside command spans. As a requirement, all
Redis connections should start after this application.

For connections started by your application, all works as expected. But some
libraries manage internally, and for those cases, you need to ensure proper
order via `extra_applications`.

One such example is [hammer_backend_redis](https://hex.pm/packages/hammer_backend_redis).
In case you depend on that library, `extra_applications` will be similar
to the following:

```elixir
  def application do
    [
      extra_applications: [:opentelemetry_redix, :hammer_backend_redis]
    ]
  end
```

## Installation

The package can be installed by adding `opentelemetry_redix` to your list of
dependencies in `mix.exs`:

```elixir
  def deps do
    [
      {:opentelemetry_redix, "~> 0.1"}
    ]
  end
```

## Compatibility Matrix

| OpentelemetryRedix Version | Otel Version | Notes |
| :------------------------- | :----------- | :---- |
|                            |              |       |
| v0.1.0                     | v1.0.0       |       |

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/opentelemetry_redix](https://hexdocs.pm/opentelemetry_redix).

