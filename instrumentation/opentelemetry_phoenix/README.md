# OpentelemetryPhoenix

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_phoenix)](https://hex.pm/packages/opentelemetry_phoenix)
![Build Status](https://github.com/opentelemetry-beam/opentelemetry_phoenix/workflows/Tests/badge.svg)

Telemetry handler that creates Opentelemetry spans from Phoenix events.

After installing, setup the handler in your application behaviour before your
top-level supervisor starts.

```elixir
OpentelemetryPhoenix.setup(adapter: :bandit)
```

See the documentation for `OpentelemetryPhoenix.setup/1` for additional options that
may be supplied.

## Installation

```elixir
def deps do
  [
    {:opentelemetry_phoenix, "~> 2.0.0-rc.2"}
  ]
end
```

[OpentelemetryBandit](https://hex.pm/packages/opentelemetry_bandit) or [OpentelemetryCowboy](https://hex.pm/packages/opentelemetry_cowboy) must be installed to capture the full
request lifecycle. Phoenix only handles part of the request lifecycle which can lead
to incomplete request durations and lost traces for requests terminated at the socket
level or before reaching Phoenix.

## Note on Phoenix integration

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

## Note on Phoenix LiveView

Phoenix LiveView async operations does not have automatic propagation. It is necessary to replace `Phoenix.LiveView.assign_async/4` and `Phoenix.LiveView.start_async/4` with `OpentelemetryPhoenix.LiveView.assign_async/4` and `OpentelemetryPhoenix.LiveView.start_async/4`.

Before:

```elixir
def mount(%{"slug" => slug}, _, socket) do
  {:ok,
   socket
   |> assign(:foo, "bar")
   |> assign_async(:org, fn -> {:ok, %{org: fetch_org!(slug)}} end)
   |> assign_async([:profile, :rank], fn -> {:ok, %{profile: ..., rank: ...}} end)}
end
```

After:

```elixir
def mount(%{"slug" => slug}, _, socket) do
  {:ok,
   socket
   |> assign(:foo, "bar")
   |> OpentelemetryPhoenix.LiveView.assign_async(:org, fn -> {:ok, %{org: fetch_org!(slug)}} end)
   |> OpentelemetryPhoenix.LiveView.assign_async([:profile, :rank], fn -> {:ok, %{profile: ..., rank: ...}} end)}
end
```

`OpentelemetryPhoenix.LiveView` must be required in all the live vie wmodules where it is used, sucha s the `live_view` and `live_component` macros:

```elixir
defmodule MyAppWeb do
  # ...
  def live_view do
    quote do
      use Phoenix.LiveView,
        layout: {MyAppWeb.Layouts, :app}

      require OpentelemetryPhoenix.LiveView

      unquote(html_helpers())
    end
  end

  def live_component do
    quote do
      use Phoenix.LiveComponent

      require OpentelemetryPhoenix.LiveView

      unquote(html_helpers())
    end
  end
end
```
