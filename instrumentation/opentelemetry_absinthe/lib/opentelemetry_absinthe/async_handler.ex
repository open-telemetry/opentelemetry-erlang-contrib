defmodule OpentelemetryAbsinthe.AsyncHandler do
  @moduledoc false

  alias OpenTelemetry.Span

  # batch events run for each batch group
  @async_task_start [:absinthe, :middleware, :async, :task, :start]
  @async_task_stop [:absinthe, :middleware, :async, :task, :stop]
  @async_task_exception [:absinthe, :middleware, :async, :task, :exception]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :async_task},
      [
        @async_task_start,
        @async_task_stop,
        @async_task_exception
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@async_task_start, _measurements, metadata, config) do
    attributes = []

    parent_ctx = OpentelemetryProcessPropagator.fetch_parent_ctx(1, :"$callers")

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      :"absinthe.middleware.async",
      metadata,
      %{attributes: attributes}
    )
  end

  def handle_event(@async_task_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@async_task_exception, _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    case metadata do
      %{kind: :error, reason: reason, stacktrace: stacktrace} ->
        Span.record_exception(ctx, reason, stacktrace)
        Span.set_status(ctx, OpenTelemetry.status(:error, format_error(reason)))

      _otherwise ->
        :ok
    end

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  defp format_error(error) when is_exception(error), do: Exception.message(error)
  defp format_error(error), do: inspect(error)
end
