# OpentelemetryXray

Implements an Id Generator compatible with [AWS X-Ray TraceId](https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids)

## Installation

The package can be installed by adding `opentelemetry_xray` to your list of
dependencies in `mix.exs` for elixir and `rebar.config` for erlang :

```erlang
{deps, [opentelemetry_xray]}.
```

```elixir
def deps do
  [
    {:opentelemetry_xray, "~> 0.1"}
  ]
end
```

## Usage

Configure the OpenTelemetry to use the X-Ray Id Generator

```elixir
  config :opentelemetry,
    id_generator: :otel_aws_xray_id_generator
```

```erlang
[
 {opentelemetry,
  [{id_generator, otel_aws_xray_id_generator}]}
].
```
