if Code.ensure_loaded(Phoenix.LiveView) do
defmodule OpentelemetryPhoenix.LiveView do
  @moduledoc """
  `OpentelemetryPhoenix.LiveView` provides a extensions to the async functions
  in the `Phoenix.LiveView` to reduce boilerplate in propagating OpenTelemetry
  contexts across process boundaries.

  > #### Module Redefinement {: .info}
  >
  > This module does not redefine the `Phoenix.Liveview` module, instead
  > it provides wrappers for async functions, so this functionality will
  > not globally modify the default behavior of the `Phoenix.Liveview` module.

  ## Usage

  Require `OpentelemetryPhoenix.LiveView` in your `live_view` and
  `live_component` macros:

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

  Update the references to `assign_async` and `start_async` to use this module:application

  ```elixir
  def mount(%{"slug" => slug}, _, socket) do
    {:ok,
    socket
    |> assign(:foo, "bar")
    |> OpentelemetryPhoenix.LiveView.assign_async(:org, fn -> {:ok, %{org: fetch_org!(slug)}} end)
    |> OpentelemetryPhoenix.LiveView.assign_async([:profile, :rank], fn -> {:ok, %{profile: ..., rank: ...}} end)}
  end
  ```
  """
  require Phoenix.LiveView

  defmacro assign_async(socket, key_or_keys, func, opts \\ []) do
    quote do
      require OpenTelemetry.Tracer

      ctx = OpenTelemetry.Ctx.get_current()

      Phoenix.LiveView.assign_async(
        unquote(socket),
        unquote(key_or_keys),
        fn ->
          OpenTelemetry.Ctx.attach(ctx)

          unquote(func).()
        end,
        unquote(opts)
      )
    end
  end

  defmacro start_async(socket, name, func, opts \\ []) do
    quote do
      require OpenTelemetry.Tracer

      ctx = OpenTelemetry.Ctx.get_current()

      Phoenix.LiveView.start_async(
        unquote(socket),
        unquote(name),
        fn ->
          OpenTelemetry.Ctx.attach(ctx)

          unquote(func).()
        end,
        unquote(opts)
      )
    end
  end
end
end
