defmodule OpentelemetryOban.PluginHandler do
  alias OpenTelemetry.Span

  @tracer_id __MODULE__

  def attach() do
    attach_plugin_start_handler()
    attach_plugin_stop_handler()
    attach_plugin_exception_handler()
  end

  defp attach_plugin_start_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_start",
      [:oban, :plugin, :start],
      &__MODULE__.handle_plugin_start/4,
      []
    )
  end

  defp attach_plugin_stop_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_stop",
      [:oban, :plugin, :stop],
      &__MODULE__.handle_plugin_stop/4,
      []
    )
  end

  defp attach_plugin_exception_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_exception",
      [:oban, :plugin, :exception],
      &__MODULE__.handle_plugin_exception/4,
      []
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

  def handle_plugin_stop(_event, _measurements, metadata, _config) do
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_plugin_exception(
        _event,
        _measurements,
        %{stacktrace: stacktrace, error: error} = metadata,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    # Record exception and mark the span as errored
    Span.record_exception(ctx, error, stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end
end
