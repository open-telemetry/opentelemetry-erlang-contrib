defmodule OpentelemetryAbsinthe.ParseHandler do
  @moduledoc false

  @parse_start [:absinthe, :parse, :start]
  @parse_stop [:absinthe, :parse, :stop]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :parse},
      [
        @parse_start,
        @parse_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@parse_start, _measurements, metadata, config) do
    attributes = []

    OpentelemetryTelemetry.start_telemetry_span(config.tracer_id, :"graphql.parse", metadata, %{
      attributes: attributes
    })
  end

  def handle_event(@parse_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end
end
