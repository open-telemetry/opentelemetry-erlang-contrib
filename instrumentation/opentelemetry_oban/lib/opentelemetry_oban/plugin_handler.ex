defmodule OpentelemetryOban.PluginHandler do
  alias OpenTelemetry.Span

  @tracer_id __MODULE__

  def attach(config) do
    attach_plugin_start_handler()
    attach_plugin_stop_handler(config)
    attach_plugin_exception_handler(config)
  end

  defp attach_plugin_start_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_start",
      [:oban, :plugin, :start],
      &__MODULE__.handle_plugin_start/4,
      []
    )
  end

  defp attach_plugin_stop_handler(config) do
    :telemetry.attach(
      "#{__MODULE__}.plugin_stop",
      [:oban, :plugin, :stop],
      &__MODULE__.handle_plugin_stop/4,
      config
    )
  end

  defp attach_plugin_exception_handler(config) do
    :telemetry.attach(
      "#{__MODULE__}.plugin_exception",
      [:oban, :plugin, :exception],
      &__MODULE__.handle_plugin_exception/4,
      config
    )
  end

  def handle_plugin_start(_event, _measurements, %{plugin: plugin} = metadata, _config) do
    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{plugin} process",
      metadata,
      %{}
    )
  end

  def handle_plugin_stop(_event, measurements, metadata, config) do
    set_measurements_attributes(measurements, config)
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_plugin_exception(
        _event,
        measurements,
        %{stacktrace: stacktrace, error: error} = metadata,
        config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    # Record exception and mark the span as errored
    Span.record_exception(ctx, error, stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))

    set_measurements_attributes(measurements, config)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp set_measurements_attributes(%{duration: duration}, %{time_unit: time_unit}) do
    OpenTelemetry.Tracer.set_attributes(%{
      :"messaging.oban.duration_#{time_unit}" =>
        System.convert_time_unit(duration, :native, time_unit)
    })
  end
end
