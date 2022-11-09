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

      with request <- Finch.build(:get, url, headers),
           {:ok, response} <- Finch.request(request, HttpFinch) do

        Tracer.set_attributes([
          {"http.url", url},
          {"http.method", request.method},
          {"http.status_code", response.status}])
        {:ok, response}
      else
        {:error, %{__exception__: true} = error} ->
          Tracer.set_status(:error, Exception.message(error))
          {:error, error}
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

