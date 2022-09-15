defmodule OpentelemetryPhoenixLiveview do
  @tracer_id __MODULE__

  alias OpenTelemetry.Span
  require OpenTelemetry.Tracer

  def setup do
    :telemetry.attach_many(
      {__MODULE__, :live_view},
      [
        [:phoenix, :live_view, :mount, :start],
        [:phoenix, :live_view, :mount, :stop],
        [:phoenix, :live_view, :mount, :exception],
        [:phoenix, :live_view, :handle_params, :start],
        [:phoenix, :live_view, :handle_params, :stop],
        [:phoenix, :live_view, :handle_params, :exception],
        [:phoenix, :live_view, :handle_event, :start],
        [:phoenix, :live_view, :handle_event, :stop],
        [:phoenix, :live_view, :handle_event, :exception],
        [:phoenix, :live_component, :handle_event, :start],
        [:phoenix, :live_component, :handle_event, :stop],
        [:phoenix, :live_component, :handle_event, :exception]
      ],
      &__MODULE__.handle_event/4,
      %{}
    )

    :ok
  end

  def handle_event(
        [:phoenix, _live, :mount, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket} = meta

    %{view: live_view} = socket

    attributes = []

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      inspect(live_view),
      meta,
      %{kind: :server}
    )

    OT.Monitor.monitor(OpenTelemetry.Tracer.current_span_ctx())

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.mount",
      meta,
      %{kind: :server}
    )
    |> Span.set_attributes(attributes)
  end

  def handle_event(
        [:phoenix, _live, :handle_params, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket} = meta

    %{view: live_view} = socket

    attributes = []

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_params",
      meta,
      %{kind: :server}
    )
    |> Span.set_attributes(attributes)
  end

  def handle_event(
        [:phoenix, _live, :handle_event, :start],
        _measurements,
        meta,
        _handler_configuration
      ) do
    %{socket: socket, event: event, params: _params} = meta

    %{view: live_view} = socket

    attributes = [event: event]

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(live_view)}.handle_event##{event}",
      meta,
      %{kind: :server}
    )
    |> Span.set_attributes(attributes)
  end

  def handle_event(
        [:phoenix, _live, _event, :stop],
        _measurements,
        meta,
        _handler_configuration
      ) do
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_event(
        [:phoenix, _live, _action, :exception],
        _,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = meta,
        _
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    exception = Exception.normalize(kind, reason, stacktrace)

    Span.record_exception(ctx, exception, stacktrace, [])
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end
end
