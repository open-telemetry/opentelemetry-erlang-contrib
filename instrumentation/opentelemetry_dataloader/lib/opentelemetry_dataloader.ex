defmodule OpentelemetryDataloader do
  @moduledoc """
  Module for automatic instrumentation of Dataloader.

  It works by listening to `[:dataloader, :source, :run/:batch, :start/:stop]` telemetry events.
  """

  @tracer_id __MODULE__

  @run_start [:dataloader, :source, :run, :start]
  @run_stop [:dataloader, :source, :run, :stop]

  @batch_start [:dataloader, :source, :batch, :run, :start]
  @batch_stop [:dataloader, :source, :batch, :run, :stop]

  @doc """
  Initializes and configures the telemetry handlers.
  """
  def setup(_opts \\ []) do
    config = %{
      tracer_id: @tracer_id
    }

    :telemetry.attach_many(
      {__MODULE__, :run},
      [
        @run_start,
        @run_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )

    :telemetry.attach_many(
      {__MODULE__, :batch},
      [
        @batch_start,
        @batch_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  def handle_event(event, measurements, metadata, config)

  def handle_event(@run_start, _measurements, metadata, config) do
    parent_ctx = OpentelemetryProcessPropagator.fetch_parent_ctx(4, :"$callers")

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      "dataloader.run",
      metadata,
      %{
        kind: :client
      }
    )
  end

  def handle_event(@run_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  def handle_event(@batch_start, _measurements, metadata, config) do
    parent_ctx = OpentelemetryProcessPropagator.fetch_parent_ctx(4, :"$callers")

    if parent_ctx != :undefined do
      OpenTelemetry.Ctx.attach(parent_ctx)
    end

    {batch_name, _batch_args} = metadata.batch

    attributes = %{
      "dataloader.batch_key" => inspect(batch_name)
    }

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      "dataloader.batch",
      metadata,
      %{
        kind: :client,
        attributes: attributes
      }
    )
  end

  def handle_event(@batch_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end
end
