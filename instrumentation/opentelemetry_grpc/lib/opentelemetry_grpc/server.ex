defmodule OpentelemetryGrpc.Server do
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.RPCAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes

  require OpenTelemetry.Tracer, as: Tracer
  require Logger

  @options_schema [
    span_relationship: [
      type: {:in, [:child, :link, :none]},
      default: :child,
      doc: """
      How spans relate to propagated parent context:
      * `:child` - Extract context and create parent-child relationships (default)
      * `:link` - Extract context and create span links for loose coupling
      * `:none` - Disable context propagation entirely
      """
    ]
  ]

  @nimble_options_schema NimbleOptions.new!(@options_schema)

  @doc false
  def options_schema do
    @options_schema
  end

  @moduledoc """
  OpenTelemetry handler for gRPC server telemetry events.

  This module handles server-side gRPC telemetry events and creates
  OpenTelemetry spans for incoming gRPC requests with proper context propagation.

  ## Usage

      # Basic setup with defaults
      OpentelemetryGrpc.Server.setup()

      # Custom configuration
      OpentelemetryGrpc.Server.setup(span_relationship: :child)
  """

  @grpc_server_tracer_id OpentelemetryGrpc.Server

  @server_events [
    [:grpc, :server, :rpc, :start],
    [:grpc, :server, :rpc, :stop],
    [:grpc, :server, :rpc, :exception]
  ]
  @grpc_rpc_system RPCAttributes.rpc_system_values().grpc
  @status_codes RPCAttributes.rpc_grpc_status_code_values()
  @status_code_mapping %{
    0 => @status_codes.ok,
    1 => @status_codes.cancelled,
    2 => @status_codes.unknown,
    3 => @status_codes.invalid_argument,
    4 => @status_codes.deadline_exceeded,
    5 => @status_codes.not_found,
    6 => @status_codes.already_exists,
    7 => @status_codes.permission_denied,
    8 => @status_codes.resource_exhausted,
    9 => @status_codes.failed_precondition,
    10 => @status_codes.aborted,
    11 => @status_codes.out_of_range,
    12 => @status_codes.unimplemented,
    13 => @status_codes.internal,
    14 => @status_codes.unavailable,
    15 => @status_codes.data_loss,
    16 => @status_codes.unauthenticated
  }
  @status_code_ok RPCAttributes.rpc_grpc_status_code_values().ok
  @status_code_internal RPCAttributes.rpc_grpc_status_code_values().internal

  @doc """
  Set up telemetry handlers for gRPC server events.

  ## Options

  #{NimbleOptions.docs(@nimble_options_schema)}

  ## Examples

      # Default setup
      OpentelemetryGrpc.Server.setup()

      # Custom configuration
      OpentelemetryGrpc.Server.setup(span_relationship: :link)

  """
  @spec setup(unquote(NimbleOptions.option_typespec(@options_schema))) :: :ok
  def setup(opts \\ []) do
    config =
      opts
      |> NimbleOptions.validate!(@nimble_options_schema)
      |> Enum.into(%{})

    :telemetry.attach_many(
      {__MODULE__, :grpc_server_handler},
      @server_events,
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event([:grpc, :server, :rpc, :start], _measurements, metadata, config) do
    span_opts = %{kind: :server, attributes: build_server_attributes(metadata)}
    links = setup_context_propagation(metadata.stream, config.span_relationship)
    span_opts = put_links(span_opts, links)

    OpentelemetryTelemetry.start_telemetry_span(
      @grpc_server_tracer_id,
      "#{metadata.stream.service_name}/#{metadata.stream.method_name}",
      metadata,
      span_opts
    )
  end

  @doc false
  def handle_event([:grpc, :server, :rpc, :stop], _measurements, metadata, _config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@grpc_server_tracer_id, metadata)
    record_server_span_result(metadata.result)
    OpentelemetryTelemetry.end_telemetry_span(@grpc_server_tracer_id, metadata)
  end

  @doc false
  def handle_event([:grpc, :server, :rpc, :exception], _measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@grpc_server_tracer_id, metadata)

    OpenTelemetry.Span.record_exception(ctx, metadata.kind, metadata.reason, metadata.stacktrace)

    record_server_span_result({:error, metadata.reason})

    OpenTelemetry.Span.set_status(
      ctx,
      OpenTelemetry.status(
        :error,
        Exception.format_banner(metadata.kind, metadata.reason, metadata.stacktrace)
      )
    )

    OpentelemetryTelemetry.end_telemetry_span(@grpc_server_tracer_id, metadata)
  end

  defp build_server_attributes(metadata) do
    %{
      RPCAttributes.rpc_system() => @grpc_rpc_system,
      RPCAttributes.rpc_service() => metadata.stream.service_name,
      RPCAttributes.rpc_method() => metadata.stream.method_name,
      NetworkAttributes.network_protocol_name() => "http",
      NetworkAttributes.network_protocol_version() => "2",
      NetworkAttributes.network_transport() => "tcp",
      "elixir.grpc.server_module" => to_string(metadata.server)
    }
    |> put_endpoint_module_attr(metadata.endpoint)
  end

  defp put_endpoint_module_attr(attrs, nil) do
    attrs
  end

  defp put_endpoint_module_attr(attrs, endpoint) do
    Map.put(attrs, "elixir.grpc.endpoint_module", to_string(endpoint))
  end

  defp record_server_span_result({:ok, _stream, _response}) do
    Tracer.set_attributes(%{
      RPCAttributes.rpc_grpc_status_code() => @status_code_ok
    })
  end

  defp record_server_span_result({:error, %GRPC.RPCError{status: status, message: message}}) do
    Tracer.set_attributes(%{
      RPCAttributes.rpc_grpc_status_code() => Map.get(@status_code_mapping, status, status),
      ErrorAttributes.error_type() => to_string(GRPC.RPCError)
    })

    case to_otel_status(status) do
      :error ->
        status_name = GRPC.Status.code_name(status)
        Tracer.set_status(OpenTelemetry.status(:error, message || "gRPC error: #{status_name}"))

      :unset ->
        :ok
    end
  end

  defp record_server_span_result({:error, %error_struct{} = error}) when is_exception(error) do
    Tracer.set_attributes(%{
      RPCAttributes.rpc_grpc_status_code() => @status_code_internal,
      ErrorAttributes.error_type() => to_string(error_struct)
    })

    Tracer.set_status(OpenTelemetry.status(:error, Exception.message(error)))
  end

  defp record_server_span_result(_result) do
    # Unknown result format - don't set status
    :ok
  end

  defp put_links(span_opts, []) do
    span_opts
  end

  defp put_links(span_opts, links) do
    Map.put(span_opts, :links, links)
  end

  defp setup_context_propagation(source, :child) do
    extract_and_attach(source)
  end

  defp setup_context_propagation(source, :link) do
    link_from_propagated_ctx(source)
  end

  defp setup_context_propagation(_source, _) do
    []
  end

  defp extract_and_attach(source) do
    case get_propagated_ctx(source) do
      {_links, parent_ctx} when parent_ctx != :undefined ->
        OpenTelemetry.Ctx.attach(parent_ctx)

        # When we attach the context, we don't need links - parent-child relationship is established
        []

      {links, _undefined_ctx} ->
        # No parent context to attach, but we can still return links if any
        links
    end
  end

  defp link_from_propagated_ctx(source) do
    {links, _ctx} = get_propagated_ctx(source)
    links
  end

  defp get_propagated_ctx(stream) do
    stream
    |> get_grpc_headers()
    |> extract_to_ctx()
  end

  defp extract_to_ctx([]) do
    {[], :undefined}
  end

  defp extract_to_ctx(headers) do
    ctx =
      OpenTelemetry.Ctx.new()
      |> :otel_propagator_text_map.extract_to(headers)

    # Extract span context to check if it's valid and for creating links
    span_ctx = OpenTelemetry.Tracer.current_span_ctx(ctx)

    case span_ctx do
      :undefined ->
        # No valid parent span - no relationship possible
        {[], :undefined}

      span_ctx ->
        # Return links first, then context (for parent-child relationships)
        {[OpenTelemetry.link(span_ctx)], ctx}
    end
  end

  defp get_grpc_headers(%GRPC.Server.Stream{http_request_headers: headers}) when is_map(headers),
    do: Map.to_list(headers)

  defp get_grpc_headers(_stream), do: []

  # https://github.com/open-telemetry/opentelemetry-specification/blob/v1.50.0/specification/trace/semantic_conventions/rpc.md
  defp to_otel_status(2), do: :error
  defp to_otel_status(4), do: :error
  defp to_otel_status(12), do: :error
  defp to_otel_status(13), do: :error
  defp to_otel_status(14), do: :error
  defp to_otel_status(15), do: :error
  defp to_otel_status(_), do: :unset
end
