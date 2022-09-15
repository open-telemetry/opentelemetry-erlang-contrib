# OpentelemetryPhoenixLiveview

This library provides integration with Phoenix Liveview and opentelemetry.

To use this library, add the following line to your `application.ex` start function:

```elixir
:ok = OpentelemetryPhoenixLiveview.setup()
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_phoenix_liveview` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_phoenix_liveview, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/opentelemetry_phoenix_liveview>.

