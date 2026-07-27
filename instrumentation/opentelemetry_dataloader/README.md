# OpentelemetryDataloader

Telemetry handler that creates Opentelemetry spans from Dataloader events.

After installing, setup the handler in your application behaviour before your
top-level supervisor starts.

```elixir
OpentelemetryDataloader.setup()
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_dataloader` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_dataloader, "~> 0.1"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/opentelemetry_dataloader>.
