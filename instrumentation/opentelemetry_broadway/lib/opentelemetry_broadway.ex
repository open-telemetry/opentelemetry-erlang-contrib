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

  ### With Trace Propagation

  For Broadway pipelines that need distributed tracing context extraction from message headers/attributes:

      def start(_type, _args) do
        :ok = OpentelemetryBroadway.setup(propagation: true)

        # ...
      end

  > #### Extracting Headers and Attributes {: .info}
  > When using trace propagation, your producer must be configured to extract headers/attributes.

  #### RabbitMQ

  For RabbitMQ, configure your `BroadwayRabbitMQ.Producer` with `metadata: [:headers]`:

      Broadway.start_link(MyBroadway,
        name: MyBroadway,
        producer: [
          module: {BroadwayRabbitMQ.Producer,
            metadata: [:headers],  # Required for trace propagation!
          }
        ]
      )

  #### Amazon SQS

  For Amazon SQS, configure your `BroadwaySQS.Producer` to extract trace context.
  By default, SQS does **not** return attributes - you must explicitly request them:

      Broadway.start_link(MyBroadway,
        name: MyBroadway,
        producer: [
          module: {BroadwaySQS.Producer,
            queue_url: "https://sqs.amazonaws.com/...",
            # For AWS X-Ray trace headers (system attribute):
            attribute_names: [:aws_trace_header],
            # For W3C Trace Context propagation (custom message attributes):
            message_attribute_names: ["traceparent", "tracestate"]
          }
        ]
      )
  """

  alias OpenTelemetry.Ctx
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemConv.Incubating.MessagingAttributes

  @tracer_id __MODULE__

  @type setup_opts :: [propagation: boolean()]

  @doc """
  Attaches the Telemetry handlers, returning `:ok` if successful.

  ## Options

  - `propagation` - Enable trace propagation from message headers

  ## Examples

      # Basic setup
      OpentelemetryBroadway.setup()

      # With trace propagation
      OpentelemetryBroadway.setup(propagation: true)

  """
  @spec setup(setup_opts()) :: :ok
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
        config
      ) do
    span_name = "#{inspect(topology_name)}/#{Atom.to_string(processor_key)} process"
    client_id = inspect(name)

    links = get_propagated_ctx(message, config)

    attributes = %{
      MessagingAttributes.messaging_system() => :broadway,
      MessagingAttributes.messaging_operation_type() => :process,
      MessagingAttributes.messaging_client_id() => client_id
    }

    attributes =
      if is_binary(message.data) do
        Map.put(
          attributes,
          MessagingAttributes.messaging_message_body_size(),
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

  defp get_propagated_ctx(message, %{propagation: true} = _config) do
    message
    |> get_message_headers()
    |> Enum.map(&normalize_header/1)
    |> Enum.reject(&is_nil/1)
    |> extract_to_ctx()
  end

  defp get_propagated_ctx(_message, _config), do: []

  defp extract_to_ctx([]) do
    []
  end

  defp extract_to_ctx(headers) do
    # Extract context into separate context to avoid polluting current context
    parent_ctx =
      :otel_propagator_text_map.extract_to(Ctx.new(), headers)
      |> Tracer.current_span_ctx()

    # Create links to parent if it exists
    case parent_ctx do
      :undefined -> []
      _ -> [OpenTelemetry.link(parent_ctx)]
    end
  end

  # RabbitMQ: headers are in metadata.headers as a list
  defp get_message_headers(%Broadway.Message{metadata: %{headers: headers}}) when is_list(headers), do: headers

  # SQS: both standard attributes and custom message attributes can contain trace context
  defp get_message_headers(%Broadway.Message{
         metadata: %{attributes: attributes, message_attributes: message_attributes}
       }) do
    message_attributes =
      message_attributes
      |> normalize_sqs_attributes()
      |> Enum.to_list()

    attributes
    |> normalize_sqs_attributes()
    |> Enum.to_list()
    |> Enum.concat(message_attributes)
  end

  defp get_message_headers(_message), do: []

  # ExAws.SQS returns:
  #
  #   - Empty: [] (list)
  #   - With data: %{"key" => "value"} for attributes
  #   - With data: %{"key" => %{name: "key", data_type: "String", value: "parsed"}} for message_attributes
  #
  # Returning an empty map as the safe fallback for empty array or
  # unrecognized data format.
  defp normalize_sqs_attributes(attrs) when is_map(attrs), do: attrs
  defp normalize_sqs_attributes(_), do: %{}

  # RabbitMQ format:
  # - {key, type, value}
  defp normalize_header({key, _type, value}) when is_binary(key) and is_binary(value), do: {key, value}
  # SQS format:
  # - {key, %{name: "key", data_type: "String", value: "..."}}
  # - {key, %{name: "key", data_type: "Binary", value: "..."}}
  defp normalize_header({key, %{name: key, value: value}}) when is_binary(value), do: {key, value}
  # Standard format: {key, value}
  defp normalize_header({key, value}) when is_binary(key) and is_binary(value), do: {key, value}
  defp normalize_header(_value), do: nil
end
