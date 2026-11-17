defmodule OpentelemetryGrpc.Server do
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes

  alias OpenTelemetry.Tracer
  alias OpentelemetryGrpc.SemConv
  alias OpentelemetryGrpc.StatusCode

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
  @status_code_ok GRPC.Status.ok()
  # the status code grpc replies with when a handler crashes
  @status_code_unknown GRPC.Status.unknown()

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
      |> Map.put(:stability, SemConv.stability())

    :telemetry.attach_many(
      {__MODULE__, :grpc_server_handler},
      @server_events,
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event([:grpc, :server, :rpc, :start], _measurements, metadata, config) do
    span_opts = %{kind: :server, attributes: build_server_attributes(metadata, config.stability)}
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
  def handle_event([:grpc, :server, :rpc, :stop], _measurements, metadata, config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@grpc_server_tracer_id, metadata)
    record_server_span_result(metadata.result, config.stability)
    OpentelemetryTelemetry.end_telemetry_span(@grpc_server_tracer_id, metadata)
  end

  @doc false
  def handle_event([:grpc, :server, :rpc, :exception], _measurements, metadata, config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@grpc_server_tracer_id, metadata)

    record_exception_result(ctx, metadata, config.stability)

    OpentelemetryTelemetry.end_telemetry_span(@grpc_server_tracer_id, metadata)
  end

  # a raised GRPC.RPCError is how a handler returns a status, so it goes through
  # the same status code mapping as a returned error
  defp record_exception_result(ctx, %{reason: %GRPC.RPCError{} = error} = metadata, stability) do
    record_server_span_result({:error, error}, stability)
    record_rpc_error_exception(to_otel_status(error.status), ctx, metadata)
  end

  defp record_exception_result(ctx, %{reason: reason} = metadata, stability)
       when is_exception(reason) do
    OpenTelemetry.Span.record_exception(ctx, reason, metadata.stacktrace)
    record_server_span_result({:error, reason}, stability)
  end

  # throw and exit carry no exception to record
  defp record_exception_result(ctx, metadata, stability) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, @status_code_unknown))

    Tracer.set_attributes(%{
      ErrorAttributes.error_type() => StatusCode.name(@status_code_unknown)
    })

    OpenTelemetry.Span.set_status(
      ctx,
      OpenTelemetry.status(
        :error,
        Exception.format_banner(metadata.kind, metadata.reason, metadata.stacktrace)
      )
    )
  end

  # only a status code that fails the span makes the raise a failed operation
  defp record_rpc_error_exception(:error, ctx, metadata) do
    OpenTelemetry.Span.record_exception(ctx, metadata.reason, metadata.stacktrace)
  end

  defp record_rpc_error_exception(:unset, _ctx, _metadata) do
    :ok
  end

  defp build_server_attributes(metadata, stability) do
    stability
    |> SemConv.rpc_attributes(metadata.stream.service_name, metadata.stream.method_name)
    |> Map.merge(%{
      NetworkAttributes.network_protocol_name() => "http",
      NetworkAttributes.network_protocol_version() => "2",
      "elixir.grpc.server_module" => to_string(metadata.server)
    })
    |> put_endpoint_module_attr(metadata.endpoint)
  end

  defp put_endpoint_module_attr(attrs, nil) do
    attrs
  end

  defp put_endpoint_module_attr(attrs, endpoint) do
    Map.put(attrs, "elixir.grpc.endpoint_module", to_string(endpoint))
  end

  defp record_server_span_result({:ok, _stream, _response}, stability) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, @status_code_ok))
  end

  defp record_server_span_result({:error, %GRPC.RPCError{} = error}, stability) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, error.status))
    record_rpc_error_status(to_otel_status(error.status), error)
  end

  defp record_server_span_result({:error, error}, stability) when is_exception(error) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, @status_code_unknown))

    Tracer.set_attributes(%{
      ErrorAttributes.error_type() => StatusCode.name(@status_code_unknown)
    })

    Tracer.set_status(OpenTelemetry.status(:error, Exception.message(error)))
  end

  defp record_server_span_result(_result, _stability) do
    # Unknown result format - don't set status
    :ok
  end

  defp record_rpc_error_status(:error, error) do
    Tracer.set_attributes(%{ErrorAttributes.error_type() => StatusCode.name(error.status)})

    Tracer.set_status(
      OpenTelemetry.status(
        :error,
        error.message || "gRPC error: #{GRPC.Status.code_name(error.status)}"
      )
    )
  end

  defp record_rpc_error_status(:unset, _error) do
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

  # matches on the map shape rather than the GRPC.Server.Stream struct: the
  # struct lives in grpc_server on grpc 1.0, which client-only consumers
  # will not have installed
  defp get_grpc_headers(%{http_request_headers: headers}) when is_map(headers),
    do: Map.to_list(headers)

  defp get_grpc_headers(_stream), do: []

  # https://github.com/open-telemetry/semantic-conventions/blob/v1.45.0/docs/rpc/grpc.md
  defp to_otel_status(2), do: :error
  defp to_otel_status(4), do: :error
  defp to_otel_status(12), do: :error
  defp to_otel_status(13), do: :error
  defp to_otel_status(14), do: :error
  defp to_otel_status(15), do: :error
  defp to_otel_status(_), do: :unset
end
