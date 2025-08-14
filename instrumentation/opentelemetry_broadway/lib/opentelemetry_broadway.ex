defmodule OpentelemetryBroadway do
  @moduledoc """
  OpenTelemetry tracing for [Broadway](https://elixir-broadway.org/) pipelines with optional trace propagation support.

  It supports job start, stop, and exception events with automatic distributed tracing context extraction.

  ## Usage

  ### Basic Setup

  In your application's `c:Application.start/2` callback:

      def start(_type, _args) do
        :ok = OpentelemetryBroadway.setup()

        # ...
      end

  ### With Trace Propagation (RabbitMQ)

  For Broadway pipelines that need distributed tracing context extraction from message headers:

      def start(_type, _args) do
        :ok = OpentelemetryBroadway.setup(propagation: true)

        # ...
      end

  **Important**: When using trace propagation, your producer must be configured to extract headers.
  For RabbitMQ, configure your `BroadwayRabbitMQ.Producer` with `metadata: [:headers]`:

      Broadway.start_link(MyBroadway,
        name: MyBroadway,
        producer: [
          module: {BroadwayRabbitMQ.Producer,
            queue: "my_queue",
            metadata: [:headers],  # Required for trace propagation!
            connection: [...]
          }
        ],
        processors: [default: [concurrency: 10]]
      )

  ## Features

  - Creates spans for Broadway message processing
  - Tracks message processing duration and outcomes
  - Records exceptions and failures with proper error status
  - **Trace Propagation**: Automatically extracts distributed tracing context from message headers
  - **Standards Compliance**: Supports W3C Trace Context and other OpenTelemetry propagation formats
  """

  alias OpenTelemetry.SemanticConventions
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemanticConventions.Trace

  require Trace

  @tracer_id __MODULE__

  @doc """
  Attaches the Telemetry handlers, returning `:ok` if successful.

  ## Options

  - `[]` or no argument - Basic Broadway instrumentation
  - `[propagation: true]` - Enable trace propagation from message headers

  ## Examples

      # Basic setup
      OpentelemetryBroadway.setup()

      # With trace propagation
      OpentelemetryBroadway.setup(propagation: true)

  """
  @spec setup :: :ok
  @spec setup(keyword()) :: :ok
  def setup(opts \\ [])

  def setup(opts) do
    opts =
      opts
      |> Enum.into(%{})
      |> Map.put_new(:propagation, true)

    :telemetry.attach(
      "#{__MODULE__}.message_start",
      [:broadway, :processor, :message, :start],
      &__MODULE__.handle_message_start/4,
      opts
    )

    :telemetry.attach(
      "#{__MODULE__}.message_stop",
      [:broadway, :processor, :message, :stop],
      &__MODULE__.handle_message_stop/4,
      opts
    )

    :telemetry.attach(
      "#{__MODULE__}.message_exception",
      [:broadway, :processor, :message, :exception],
      &__MODULE__.handle_message_exception/4,
      opts
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
        opts
      ) do
    span_name = "#{inspect(topology_name)}/#{Atom.to_string(processor_key)} process"
    client_id = inspect(name)

    # Extract trace context if propagation is enabled
    {parent_ctx, links} = extract_trace_context(message, opts)

    # Clear current span context and set up links if we have a parent context
    if parent_ctx != :undefined do
      OpenTelemetry.Tracer.set_current_span(:undefined)
    end

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
      links: links,
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

  defp format_error(err) when is_binary(err), do: err
  defp format_error(err), do: inspect(err)

  # Extract trace context from message headers if propagation is enabled
  defp extract_trace_context(message, %{propagation: true}) do
    case get_message_headers(message) do
      headers when is_list(headers) and length(headers) > 0 ->
        # Convert headers to a format suitable for otel_propagator_text_map
        header_map = convert_headers_to_map(headers)

        if Enum.empty?(header_map) do
          {:undefined, []}
        else
          # Extract trace context using OpenTelemetry propagator
          parent_ctx =
            :otel_propagator_text_map.extract_to(OpenTelemetry.Ctx.new(), header_map)
            |> OpenTelemetry.Tracer.current_span_ctx()

          links = if parent_ctx != :undefined, do: [OpenTelemetry.link(parent_ctx)], else: []
          {parent_ctx, links}
        end

      _ ->
        {:undefined, []}
    end
  end

  defp extract_trace_context(_message, _opts), do: {:undefined, []}

  # Get headers from message metadata
  defp get_message_headers(%Broadway.Message{metadata: %{headers: headers}}) when is_list(headers), do: headers
  defp get_message_headers(_message), do: []

  # Convert various header formats to string key-value pairs
  defp convert_headers_to_map(headers) do
    headers
    |> Enum.map(fn
      # RabbitMQ format: {key, type, value}
      {key, _type, value} when is_binary(key) and is_binary(value) -> {key, value}
      {key, _type, value} when is_binary(key) -> {key, to_string(value)}
      # Simple key-value pairs
      {key, value} when is_binary(key) and is_binary(value) -> {key, value}
      {key, value} when is_binary(key) -> {key, to_string(value)}
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
  end
end
