# OpentelemetryCommanded

Telemetry handler that creates OpenTelemetry spans from [Commanded](https://github.com/commanded/commanded) commands and events.

## Supported spans

OpentelemetryCommanded currently creates spans for:

- Application Dispatch
- Aggregate Execute
- Event.Handler Handle
- ProcessManager Handle
- EventStore `append_to_stream` and `stream_forward`

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_commanded` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_commanded, "~> 0.1.0"}
  ]
end
```

Once installed, execute the following function in your application behaviour before your top-level supervisor starts.

```elixir
# lib/my_app/application.ex
OpentelemetryCommanded.setup()
```

Then add the `OpentelemetryCommanded.Middleware` to your `Commanded` routers

```elixir
middleware OpentelemetryCommanded.Middleware
```

## Documentation

[https://hexdocs.pm/opentelemetry_commanded](https://hexdocs.pm/opentelemetry_commanded).
