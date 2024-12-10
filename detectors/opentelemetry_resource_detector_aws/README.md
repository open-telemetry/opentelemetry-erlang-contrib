# OpentelemetryResourceDetectorAWS

Implements a Resource Detector for AWS.

## Installation

The package can be installed by adding `opentelemetry_resource_detector_aws` to your list of
dependencies in `mix.exs` for elixir and `rebar.config` for erlang :

```erlang
{deps, [opentelemetry_resource_detector_aws]}.
```

```elixir
def deps do
  [
    {:opentelemetry_resource_detector_aws, "~> 0.1"}
  ]
end
```

## Usage

Configure the OpenTelemetry to use the Resource Detector for AWS:

```elixir
config :opentelemetry,
    resource_detectors: [:otel_resource_aws_ecs]
```

```erlang
[
 {opentelemetry,
  [{resource_detectors, otel_resource_aws_ecs}]}
].
```
