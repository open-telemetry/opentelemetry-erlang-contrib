defmodule OpentelemetryBroadway do
  @moduledoc """
  OpenTelemetry tracing for [Broadway](https://elixir-broadway.org/) pipelines.

  It supports job start, stop, and exception events.

  ## Usage

  In your application's `c:Application.start/2` callback:

      def start(_type, _args) do
        :ok = OpentelemetryBroadway.setup()

        # ...
      end

  """

  alias OpenTelemetry.SemanticConventions
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemanticConventions.Trace

  require Trace

  @tracer_id __MODULE__

  @doc """
  Attaches the Telemetry handlers, returning `:ok` if successful.
  """
  @spec setup :: :ok
  def setup do
    :ok =
      :telemetry.attach(
        "#{__MODULE__}.message_start",
        [:broadway, :processor, :message, :start],
        &__MODULE__.handle_message_start/4,
        []
      )

    :ok =
      :telemetry.attach(
        "#{__MODULE__}.message_stop",
        [:broadway, :processor, :message, :stop],
        &__MODULE__.handle_message_stop/4,
        []
      )

    :ok =
      :telemetry.attach(
        "#{__MODULE__}.job_exception",
        [:broadway, :processor, :message, :exception],
        &__MODULE__.handle_message_exception/4,
        []
      )

    :ok =
      :telemetry.attach(
        "#{__MODULE__}.batch_start",
        [:broadway, :batch_processor, :start],
        &__MODULE__.handle_batch_start/4,
        []
      )

    :ok =
      :telemetry.attach(
        "#{__MODULE__}.batch_stop",
        [:broadway, :batch_processor, :stop],
        &__MODULE__.handle_batch_stop/4,
        []
      )

    :ok
  end

  @doc false
  def handle_message_start(
        _event,
        _measurements,
        %{
          processor_key: processor_key,
          topology_name: topology_name,
          name: name,
          message: %Broadway.Message{} = message
        } = metadata,
        _config
      ) do
    span_name = "#{inspect(topology_name)}/#{Atom.to_string(processor_key)} process"
    client_id = inspect(name)

    attributes = %{
      SemanticConventions.Trace.messaging_system() => :broadway,
      SemanticConventions.Trace.messaging_operation() => :process,
      SemanticConventions.Trace.messaging_consumer_id() => client_id
    }

    attributes =
      if is_binary(message.data) do
        Map.put(
          attributes,
          SemanticConventions.Trace.messaging_message_payload_size_bytes(),
          byte_size(message.data)
        )
      else
        attributes
      end

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, %{
      kind: :consumer,
      attributes: attributes
    })
  end

  @doc false
  def handle_message_stop(
        _event,
        _measurements,
        %{message: %Broadway.Message{} = message} = metadata,
        _config
      ) do
    status =
      case message.status do
        :ok -> OpenTelemetry.status(:ok)
        {:failed, err} -> OpenTelemetry.status(:error, format_error(err))
      end

    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)
    OpenTelemetry.Span.set_status(ctx, status)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  @doc false
  def handle_message_exception(
        _event,
        _measurements,
        %{
          kind: kind,
          reason: reason,
          stacktrace: stacktrace
        } = metadata,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    # Record exception and mark the span as errored
    Span.record_exception(ctx, reason, stacktrace)

    Span.set_status(
      ctx,
      OpenTelemetry.status(:error, Exception.format_banner(kind, reason, stacktrace))
    )

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  @doc false
  def handle_batch_start(
    _event,
    _measurements,
    %{
      topology_name: topology_name,
      messages: messages,
      batch_info: %Broadway.BatchInfo{batcher: batcher},
    } = metadata,
    _config
  ) do
    span_name = "#{inspect(topology_name)}/#{Atom.to_string(batcher)} batching process"

    attributes = %{
      :"broadway.batch.count" => length(messages)
    }

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, %{
      kind: :internal,
      attributes: attributes
    })
  end

  @doc false
  def handle_batch_stop(
    _event,
    _measurements,
    %{
      successful_messages: successful_messages,
      failed_messages: failed_messages
    } = metadata,
    _config
  ) do
    status = if Enum.empty?(failed_messages) do
      OpenTelemetry.status(:ok)
    else
      OpenTelemetry.status(:error, "Batch completed with failed messages")
    end

    attributes = %{
      :"broadway.batch.success.count" => length(successful_messages),
      :"broadway.batch.failed.count" => length(failed_messages)
    }

    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    OpenTelemetry.Span.set_status(ctx, status)
    OpenTelemetry.Span.set_attributes(ctx, attributes)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp format_error(err) when is_binary(err), do: err
  defp format_error(err), do: inspect(err)
end
