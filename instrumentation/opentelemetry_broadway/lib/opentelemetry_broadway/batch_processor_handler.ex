defmodule OpentelemetryBroadway.BatchProcessorHandler do
  @moduledoc false

  @batch_processor_start [:broadway, :batch_processor, :start]
  @batch_processor_stop [:broadway, :batch_processor, :stop]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :batch_processor},
      [
        @batch_processor_start,
        @batch_processor_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@batch_processor_start, _measurements, metadata, config) do
    span_name =
      "#{inspect(metadata.topology_name)}-BatchProcessor-#{metadata.batch_info.batcher} process"

    links = OpentelemetryBroadway.links_from(metadata.messages)

    attributes =
      %{
        "broadway.topology_name": metadata.topology_name |> inspect(),
        "broadway.stage": :batch_processor,
        "broadway.index": metadata.index,
        "broadway.batcher": metadata.batch_info.batcher,
        "broadway.batch_key": metadata.batch_info.batch_key |> inspect(),
        "broadway.batch_size": metadata.batch_info.size
      }
      |> maybe_put(:"broadway.batch_partition", metadata.batch_info.partition)

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, span_name, metadata, %{
      links: links,
      attributes: attributes
    })
  end

  def handle_event(@batch_processor_stop, _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    OpenTelemetry.Span.set_attributes(ctx, %{
      "broadway.successful_messages_count": length(metadata.successful_messages),
      "broadway.failed_messages_count": length(metadata.failed_messages)
    })

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  defp maybe_put(attributes, _key, nil), do: attributes
  defp maybe_put(attributes, key, value), do: Map.put(attributes, key, value)
end
