defmodule OpentelemetryCommanded.Middleware do
  @moduledoc """
  A middleware for propagating span context to Aggregates, Event Handlers, etc

  Usage:

  ```elixir
  # In your commanded router

  middleware OpentelemetryCommanded.Middleware
  ```
  """

  @behaviour Commanded.Middleware

  require OpenTelemetry.Tracer

  import Commanded.Middleware.Pipeline
  import OpentelemetryCommanded.Util

  alias Commanded.Middleware.Pipeline

  def before_dispatch(%Pipeline{} = pipeline) do
    trace_headers = :otel_propagator_text_map.inject([])

    assign_metadata(pipeline, "trace_ctx", encode_headers(trace_headers))
  end

  def after_dispatch(pipeline) do
    pipeline
  end

  def after_failure(pipeline) do
    pipeline
  end
end
