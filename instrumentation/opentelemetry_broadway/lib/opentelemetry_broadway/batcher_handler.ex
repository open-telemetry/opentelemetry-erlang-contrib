defmodule OpentelemetryBroadway.BatcherHandler do
  @moduledoc false

  @batcher_start [:broadway, :batcher, :start]
  @batcher_stop [:broadway, :batcher, :stop]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :batcher},
      [
        @batcher_start,
        @batcher_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@batcher_start, _measurements, metadata, config) do
    span_name = "#{inspect(metadata.topology_name)}-Batcher-#{metadata.batcher_key} process"

    links = OpentelemetryBroadway.links_from(metadata.messages)

    attributes = %{
      "broadway.topology_name": metadata.topology_name |> inspect(),
      "broadway.stage": :batcher,
      "broadway.batcher": metadata.batcher_key,
      "broadway.batch_key": metadata.batcher_key,
      "broadway.messages_count": length(metadata.messages)
    }

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, span_name, metadata, %{
      links: links,
      attributes: attributes
    })
  end

  def handle_event(@batcher_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end
end
