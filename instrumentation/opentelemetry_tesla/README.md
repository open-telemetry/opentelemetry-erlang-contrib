# OpentelemetryTesla

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_tesla)](https://hex.pm/packages/opentelemetry_tesla)

[Tesla](https://hex.pm/packages/tesla) middleware for OpenTelemetry instrumentation and trace propagation.

See [Docs](https://hexdocs.pm/opentelemetry_tesla) for usage instructions.

## Installation

```elixir
def deps do
  [
    {:opentelemetry_tesla, "~> 2.5"}
  ]
end
```

## Setup

Add the OpenTelemetry middleware to your Tesla client. **The order of middlewares is important:**

- `Tesla.Middleware.OpenTelemetry` should come **before** `Tesla.Middleware.PathParams` (if used)

This ensures the span name includes the parameterized path (e.g., `/api/users/:id`) instead of the actual path (e.g., `/api/users/123`).

```elixir
client =
  Tesla.client([
    {Tesla.Middleware.OpenTelemetry, opt_in_attrs: [URLAttributes.url_template()]},
    Tesla.Middleware.PathParams
  ])

Tesla.get(client, "/users/:id", opts: [path_params: [id: "123"]])
```
