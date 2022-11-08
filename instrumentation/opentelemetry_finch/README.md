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

*Since this instrumentation is based on telemetry, it is not possible to automatically propagate the context in the http headers. If you need to perform context propagation, you should opt for manual instrumentation. Something like this:*

```elixir
    require OpenTelemetry.Tracer, as: Tracer

    Tracer.with_span "HTTP #{url}" do
      headers = :otel_propagator_text_map.inject([])

      case Finch.build(:get, url, headers) |> Finch.request(HttpFinch) do
        {:ok, %Finch.Response{body: body}} -> {:ok, body}
        error -> error
      end
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

