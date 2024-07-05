defmodule OpentelemetryBroadway do
  @moduledoc """
  OpentelemetryBroadway uses `telemetry` handlers to create `OpenTelemetry` spans
  from Broadway command events.
  """
  alias OpentelemetryBroadway.BatchProcessorHandler
  alias OpentelemetryBroadway.BatcherHandler
  alias OpentelemetryBroadway.ProcessorHandler
  alias OpentelemetryBroadway.RabbitMQHandler
  alias OpentelemetryBroadway.SQSHandler

  @tracer_id __MODULE__

  @doc """
  Initializes and configures telemetry handlers.

  Example:

      OpentelemetryBroadway.setup()
  """
  def setup(_opts \\ []) do
    config = %{
      tracer_id: @tracer_id
    }

    # Broadway Topology
    ProcessorHandler.attach(config)
    BatcherHandler.attach(config)
    BatchProcessorHandler.attach(config)

    # Producers
    RabbitMQHandler.attach(config)
    SQSHandler.attach(config)

    :ok
  end

  def start_link(module, opts) do
    Broadway.start_link(module, Keyword.update!(opts, :producer, &producer/1))
  end

  defp producer(opts) do
    case Keyword.get(opts, :module) do
      {BroadwayKafka.Producer, args} ->
        args = Keyword.put_new(args, :client, OpentelemetryBroadwayKafka.Client)

        opts
        |> Keyword.put(:module, {BroadwayKafka.Producer, args})
        |> Keyword.put_new(:transformer, {OpentelemetryBroadwayKafka, :instrument, []})

      {BroadwayRabbitMQ.Producer, args} ->
        args =
          args
          |> Keyword.put_new(:metadata, [])
          |> Keyword.update!(:metadata, &(&1 ++ [:headers]))

        opts
        |> Keyword.put(:module, {BroadwayRabbitMQ.Producer, args})
        |> Keyword.put_new(:transformer, {OpentelemetryBroadwayRabbitMQ, :instrument, []})

      _unknown ->
        opts
    end
  end

  @propagator_key :"$__opentelemetry_broadway_propagator_key"

  def propagate(message) do
    span_ctx = OpenTelemetry.Tracer.current_span_ctx()
    metadata = Map.put(message.metadata, @propagator_key, span_ctx)

    %{message | metadata: metadata}
  end

  @doc false
  def inject_into(%{metadata: metadata} = message, headers) do
    span_ctx =
      :otel_ctx.new()
      |> :otel_propagator_text_map.extract_to(headers)
      |> :otel_tracer.current_span_ctx()

    metadata = Map.put(metadata, @propagator_key, span_ctx)
    %{message | metadata: metadata}
  end

  @doc false
  def links_from(messages) do
    messages
    |> Enum.map(&Map.get(&1.metadata, @propagator_key, :undefined))
    |> OpenTelemetry.links()
  end
end
