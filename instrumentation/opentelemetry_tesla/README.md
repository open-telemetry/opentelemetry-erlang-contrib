# OpenTelemetryTesla

Tesla middleware that creates OpenTelemetry spans and injects tracing headers into HTTP requests for Tesla clients.


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `opentelemetry_tesla` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_tesla, "~> 2.0.1"}
  ]
end
```

## Setup

Whilst using this middleware is as simple as adding it to your Tesla middlewares configuration, **It's very important to set the correct order of the middlewares**

The is crucial to correctly get the parameterized version of the URL, something like `/api/users/:id` instead of `/api/users/3`. 

`OpenTelemetry` comes **first**, `PathParams` (if you're using it) comes after.

```elixir
Tesla.Middleware.OpenTelemetry
Tesla.Middleware.PathParams
```

