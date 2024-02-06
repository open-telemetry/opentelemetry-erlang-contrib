# OpentelemetryBandit

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_bandit)](https://hex.pm/packages/opentelemetry_bandit)
![Build Status](https://github.com/opentelemetry-beam/opentelemetry_bandit/workflows/Tests/badge.svg)

Telemetry handler that creates Opentelemetry spans from [Bandit events](https://hexdocs.pm/bandit/Bandit.Telemetry.html#content).

After installing, setup the handler in your application behaviour before your top-level supervisor starts.

```elixir
OpentelemetryBandit.setup()
```

When phoenix is used, setup telemetry this way:

```elixir
OpentelemetryBandit.setup()
OpentelemetryPhoenix.setup(adapter: :bandit)
```

## Compatibility Matrix

| OpentelemetryPhoenix Version | Otel Version | Notes |
| :--------------------------- | :----------- | :---- |
|                              |              |       |
| v0.1.4                       | v1.0         |       |

## Installation

```elixir
def deps do
  [
    {:opentelemetry_bandit, "~> 0.1.4"}
  ]
end
```
