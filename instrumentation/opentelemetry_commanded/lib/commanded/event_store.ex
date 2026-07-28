defmodule OpentelemetryCommanded.EventStore do
  @moduledoc false

  require OpenTelemetry.Tracer

  import OpentelemetryCommanded.Util

  alias OpenTelemetry.Span

  @tracer_id __MODULE__

  def setup do
    ~w(
      ack_event
      adapter
      append_to_stream
      delete_snapshot
      delete_subscription
      read_snapshot
      record_snapshot
      stream_forward
      stream_forward
      stream_forward
      subscribe
      subscribe_to
      subscribe_to
      unsubscribe
    )a
    |> Enum.each(fn event ->
      :telemetry.attach(
        {__MODULE__, :start},
        [:commanded, :event_store, event, :start],
        &__MODULE__.handle_start/4,
        []
      )

      :telemetry.attach(
        {__MODULE__, :stop},
        [:commanded, :event_store, event, :stop],
        &__MODULE__.handle_stop/4,
        []
      )

      :telemetry.attach(
        {__MODULE__, :exception},
        [:commanded, :event_store, event, :exception],
        &__MODULE__.handle_exception/4,
        []
      )
    end)
  end

  def handle_start([_, _, action, _type], _measurements, meta, _) do
    event = meta.event

    safe_context_propagation(event.metadata["trace_ctx"])

    attributes = [
      "commanded.application": meta.application,
      "commanded.causation_id": event.causation_id,
      "commanded.correlation_id": event.correlation_id,
      "commanded.event": event.event_type,
      "commanded.event_id": event.event_id,
      "commanded.event_number": event.event_number,
      "commanded.stream_id": event.stream_id,
      "commanded.stream_version": event.stream_version
    ]

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "commanded.event_store.#{action}",
      meta,
      %{
        kind: :internal,
        attributes: attributes
      }
    )
  end

  def handle_stop(_event, _measurements, meta, _) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    if error = meta[:error] do
      Span.set_status(ctx, OpenTelemetry.status(:error, inspect(error)))
    end

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_exception(
        _event,
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = meta,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    # try to normalize all errors to Elixir exceptions
    exception = Exception.normalize(kind, reason, stacktrace)

    # record exception and mark the span as errored
    Span.record_exception(ctx, exception, stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, inspect(reason)))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end
end
