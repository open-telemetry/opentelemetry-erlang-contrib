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
    {:opentelemetry_phoenix, "~> 1.2"}
  ]
end
```

It is high recommended to also install [OpentelemetryCowboy](https://hex.pm/packages/opentelemetry_cowboy) to capture the full
request lifecycle. Phoenix only handles part of the request lifecycle which can lead
to incomplete request durations and lost traces for requests terminated at the socket
level or before reaching Phoenix.

## Compatibility Matrix

| OpentelemetryPhoenix Version | Otel Version | Notes |
| :--------------------------- | :----------- | :---- |
|                              |              |       |
| v0.1.0                       | <= v.0.5.0   |       |
| v1.0.0-rc.3                  | v1.0.0-rc.1  |       |
|                              | v1.0.0-rc.2  |       |
| v1.0.0-rc.4                  | v1.0.0-rc.2  | Otel rc.3 will be a breaking change |
| v1.0.0-rc.5                  | v1.0.0-rc.3  |       |
| v1.0.0-rc.6                  | v1.0.0-rc.4  |       |
| v1.0                         | v1.0         |       |

## Note on phoenix integration

`OpentelemetryPhoenix` requires phoenix to use `Plug.Telemetry` in order to correctly trace endpoint calls.

The `endpoint.ex` file should look like:
```Elixir
defmodule MyApp.Endpoint do
  use Phoenix.Endpoint, otp_app: :my_app
  ...
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]
  ...
end
```
The [Phoenix endpoint.ex template](https://github.com/phoenixframework/phoenix/blob/v1.6.0/installer/templates/phx_web/endpoint.ex#L39) can be used as a reference

