# OpentelemetryExAws

OpentelemetryExAws uses [telemetry](https://hex.pm/packages/telemetry/)
handlers to create OpenTelemetry spans from
[ExAws](https://hex.pm/packages/ex_aws) events.

## Installation

The package can be installed by adding `opentelemetry_ex_aws` to your list of
dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_ex_aws, "~> 0.1.0"}
  ]
end
```

In your application start:

```elixir
def start(_type, _args) do
  OpentelemetryExAws.setup()

  # ...
end
```

You may also provide a custom prefix, if you have configured ExAws to use one:

```elixir
def start(_type, _args) do
  OpentelemetryExAws.setup([:ex_aws, :custom_prefix])

  # ...
end
```

## Compatibility Matrix

| opentelemetry_ex_aws Version | Otel version | Notes |
|:-----------------------------|:-------------|:------|
| v0.1.0                       | v1.0.0       |       |

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/opentelemetry_ex_aws>.

