defmodule OpentelemetryAbsinthe do
  @moduledoc """
  Module for automatic instrumentation of Absinthe resolution.

  It works by listening to [:absinthe, :execute, :operation, :start/:stop] telemetry events,
  which are emitted by Absinthe only since v1.5; therefore it won't work on previous versions.

  (you can still call `OpentelemetryAbsinthe.Instrumentation.setup()` in your application startup
  code, it just won't do anything.)
  """
  alias OpentelemetryAbsinthe.AsyncHandler
  alias OpentelemetryAbsinthe.BatchHandler
  alias OpentelemetryAbsinthe.ExecuteHandler
  alias OpentelemetryAbsinthe.ParseHandler
  alias OpentelemetryAbsinthe.ValidateHandler

  @tracer_id __MODULE__

  @doc """
  Initializes and configures the telemetry handlers.
  """
  def setup(_opts \\ []) do
    config = %{
      tracer_id: @tracer_id
    }

    # Base GraphQL events
    ParseHandler.attach(config)
    ValidateHandler.attach(config)
    ExecuteHandler.attach(config)

    # Absinthe specific events
    AsyncHandler.attach(config)
    BatchHandler.attach(config)
  end
end
