# OpentelemetryPhoenix

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_phoenix)](https://hex.pm/packages/opentelemetry_phoenix)
![Build Status](https://github.com/opentelemetry-beam/opentelemetry_phoenix/workflows/Tests/badge.svg)

Telemetry handler that creates Opentelemetry spans from Phoenix events.

After installing, setup the handler in your application behaviour before your
top-level supervisor starts.

```elixir
OpentelemetryPhoenix.setup()
```

See the documentation for `OpentelemetryPhoenix.setup/1` for additional options that
may be supplied.


## Installation

```elixir
def deps do
  [
    {:opentelemetry_phoenix, "~> 1.0.0-rc"}
  ]
end
```

