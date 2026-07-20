defmodule OpentelemetryAbsinthe.BatchHandler do
  @moduledoc false

  alias OpenTelemetry.Span
  alias OpentelemetryAbsinthe.SpanTracker

  # batch events run for each batch group
  @batch_start [:absinthe, :middleware, :batch, :start]
  @batch_stop [:absinthe, :middleware, :batch, :stop]
  @batch_exception [:absinthe, :middleware, :batch, :exception]

  # batch post events run once for each resolver, to extract data from batch
  @batch_post_start [:absinthe, :middleware, :batch, :post, :start]
  @batch_post_stop [:absinthe, :middleware, :batch, :post, :stop]
  @batch_post_exception [:absinthe, :middleware, :batch, :post, :exception]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :batch},
      [
        @batch_start,
        @batch_stop,
        @batch_exception
      ],
      &__MODULE__.handle_event/4,
      config
    )

    :telemetry.attach_many(
      {__MODULE__, :batch_post},
      [
        @batch_post_start,
        @batch_post_stop,
        @batch_post_exception
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@batch_start, _measurements, metadata, config) do
    {module, function} =
      case metadata.batch_fun do
        {m, f} -> {m, f}
        {m, f, _} -> {m, f}
      end

    attributes = [
      "absinthe.middleware.batch.module": inspect(module),
      "absinthe.middleware.batch.function": "#{function}/2"
    ]

    parent_ctx = SpanTracker.root_parent_ctx()

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      :"absinthe.middleware.batch",
      metadata,
      %{
        attributes: attributes
      }
    )
  end

  def handle_event(@batch_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@batch_exception, _measurements, metadata, config) do
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

  def handle_event(@batch_post_start, _measurements, metadata, config) do
    resolution = metadata.resolution
    path = Absinthe.Resolution.path(resolution)

    attributes = [
      "absinthe.middleware.batch.batch_key": inspect(metadata.batch_key),
      "absinthe.middleware.batch.post_batch_fun": inspect(metadata.post_batch_fun)
    ]

    parent_ctx = SpanTracker.find_parent_ctx(path)

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      :"absinthe.middleware.batch.post",
      metadata,
      %{
        attributes: attributes
      }
    )
  end

  def handle_event(@batch_post_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@batch_post_exception, _measurements, metadata, config) do
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
