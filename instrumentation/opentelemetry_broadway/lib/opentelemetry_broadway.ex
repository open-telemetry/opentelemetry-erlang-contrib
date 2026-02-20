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
        :ok = OpentelemetryBroadway.setup(span_relationship: :link)

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

  @options_schema [
    span_relationship: [
      type: {:in, [:child, :link, :none]},
      default: :link,
      doc: """
      How spans relate to propagated parent context:
      * `:child` - Extract context and create parent-child relationships
      * `:link` - Extract context and create span links for loose coupling (default)
      * `:none` - Disable context propagation entirely
      """
    ],
    propagation: [
      type: :boolean,
      deprecated: "Use `:span_relationship` instead. `propagation: true` maps to `span_relationship: :link`",
      doc: """
      When `true`, enables context propagation with span links (maps to `span_relationship: :link`).
      When `false`, disables context propagation (maps to `span_relationship: :none`).
      """
    ]
  ]

  @nimble_options_schema NimbleOptions.new!(@options_schema)

  @doc """
  Attaches the Telemetry handlers, returning `:ok` if successful.

  ## Options

  #{NimbleOptions.docs(@nimble_options_schema)}

  ## Examples

      # Basic setup (defaults to :link)
      OpentelemetryBroadway.setup()

      # With parent-child relationships
      OpentelemetryBroadway.setup(span_relationship: :child)

      # With span links (loose coupling)
      OpentelemetryBroadway.setup(span_relationship: :link)

      # Disable context propagation
      OpentelemetryBroadway.setup(span_relationship: :none)

  """
  @spec setup(unquote(NimbleOptions.option_typespec(@options_schema))) :: :ok
  def setup(opts \\ []) when is_list(opts) do
    config =
      opts
      |> validate_deprecated_options()
      |> NimbleOptions.validate!(@nimble_options_schema)
      |> Enum.into(%{})

    :telemetry.attach(
      "#{__MODULE__}.message_start",
      [:broadway, :processor, :message, :start],
      &__MODULE__.handle_message_start/4,
      config
    )

    :telemetry.attach(
      "#{__MODULE__}.message_stop",
      [:broadway, :processor, :message, :stop],
      &__MODULE__.handle_message_stop/4,
      config
    )

    :telemetry.attach(
      "#{__MODULE__}.message_exception",
      [:broadway, :processor, :message, :exception],
      &__MODULE__.handle_message_exception/4,
      config
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

    span_opts = %{kind: :consumer, attributes: build_message_attributes(message, client_id)}
    links = setup_context_propagation(message, config.span_relationship)
    span_opts = put_links(span_opts, links)

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, span_opts)
  end

  @doc false
  def handle_message_stop(
        _event,
        _measurements,
        %{message: %Broadway.Message{} = message} = metadata,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)
    Span.set_status(ctx, otel_status(message))
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

  defp otel_status(%{status: :ok}), do: OpenTelemetry.status(:ok)
  defp otel_status(%{status: {:failed, err}}), do: OpenTelemetry.status(:error, format_error(err))
  defp otel_status(_), do: OpenTelemetry.status(:unset)

  defp build_message_attributes(%Broadway.Message{}, client_id) do
    %{
      MessagingAttributes.messaging_system() => :broadway,
      MessagingAttributes.messaging_operation_type() => :process,
      MessagingAttributes.messaging_client_id() => client_id
    }
  end

  defp format_error(err) when is_binary(err), do: err
  defp format_error(err), do: inspect(err)

  # Backwards compatibility: map deprecated `propagation` option to `span_relationship`
  defp validate_deprecated_options(opts) do
    if Keyword.has_key?(opts, :propagation) and Keyword.has_key?(opts, :span_relationship) do
      raise ArgumentError,
            "cannot use both :propagation and :span_relationship options. " <>
              "Please use :span_relationship only as :propagation is deprecated"
    end

    case Keyword.pop(opts, :propagation) do
      {true, opts} -> Keyword.put(opts, :span_relationship, :link)
      {false, opts} -> Keyword.put(opts, :span_relationship, :none)
      {nil, opts} -> opts
    end
  end

  # Context propagation helpers - following OpentelemetryGrpc.Server pattern

  defp put_links(span_opts, []) do
    span_opts
  end

  defp put_links(span_opts, links) do
    Map.put(span_opts, :links, links)
  end

  defp setup_context_propagation(message, :child) do
    extract_and_attach(message)
  end

  defp setup_context_propagation(message, :link) do
    link_from_propagated_ctx(message)
  end

  defp setup_context_propagation(_message, :none) do
    []
  end

  defp extract_and_attach(message) do
    case get_propagated_ctx(message) do
      {_links, parent_ctx} when parent_ctx != :undefined ->
        Ctx.attach(parent_ctx)
        # When we attach the context, we don't need links - parent-child relationship is established
        []

      {links, _undefined_ctx} ->
        # No parent context to attach, but we can still return links if any
        links
    end
  end

  defp link_from_propagated_ctx(message) do
    {links, _ctx} = get_propagated_ctx(message)
    links
  end

  defp get_propagated_ctx(message) do
    message
    |> get_message_headers()
    |> Enum.map(&normalize_header/1)
    |> Enum.reject(&is_nil/1)
    |> extract_to_ctx()
  end

  defp extract_to_ctx([]) do
    {[], :undefined}
  end

  defp extract_to_ctx(headers) do
    ctx =
      Ctx.new()
      |> :otel_propagator_text_map.extract_to(headers)

    # Extract span context to check if it's valid and for creating links
    span_ctx = Tracer.current_span_ctx(ctx)

    case span_ctx do
      :undefined ->
        # No valid parent span - no relationship possible
        {[], :undefined}

      span_ctx ->
        # Return links first, then context (for parent-child relationships)
        {[OpenTelemetry.link(span_ctx)], ctx}
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
