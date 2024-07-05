defmodule OpentelemetryBroadway.ProcessorHandler do
  @moduledoc false

  @processor_start [:broadway, :processor, :start]
  @processor_stop [:broadway, :processor, :stop]

  @message_start [:broadway, :processor, :message, :start]
  @message_stop [:broadway, :processor, :message, :stop]
  @message_exception [:broadway, :processor, :message, :exception]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :processor},
      [
        @processor_start,
        @processor_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )

    :telemetry.attach_many(
      {__MODULE__, :processor_message},
      [
        @message_start,
        @message_stop,
        @message_exception
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@processor_start, _measurements, metadata, config) do
    span_name = "#{inspect(metadata.topology_name)}-Processor-#{metadata.processor_key} process"

    links = OpentelemetryBroadway.links_from(metadata.messages)

    attributes = %{
      "broadway.topology_name": metadata.topology_name |> inspect(),
      "broadway.stage": :processor,
      "broadway.index": metadata.index,
      "broadway.processor_key": metadata.processor_key,
      "broadway.messages_count": length(metadata.messages)
    }

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, span_name, metadata, %{
      links: links,
      attributes: attributes
    })
  end

  def handle_event(@processor_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@message_start, _measurements, metadata, config) do
    span_name = "#{inspect(metadata.topology_name)}-Processor-#{metadata.processor_key} message"

    links = OpentelemetryBroadway.links_from([metadata.message])

    attributes = %{}

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, span_name, metadata, %{
      links: links,
      attributes: attributes
    })
  end

  def handle_event(@message_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@message_exception, _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    OpenTelemetry.Span.record_exception(ctx, metadata.reason, metadata.stacktrace)
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, format_error(metadata.reason)))

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  defp format_error(exception) when is_exception(exception), do: Exception.message(exception)
  defp format_error(error), do: inspect(error)
end
