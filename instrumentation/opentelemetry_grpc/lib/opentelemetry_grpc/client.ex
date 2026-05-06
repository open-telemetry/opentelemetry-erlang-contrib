defmodule OpentelemetryGrpc.Client do
  @moduledoc """
  OpenTelemetry handler for gRPC client telemetry events.

  Attaches telemetry handlers for gRPC client events, automatically creating
  OpenTelemetry spans for outgoing RPC calls following OpenTelemetry semantic
  conventions.
  """

  alias OpenTelemetry.SemConv.Incubating.RPCAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.ErrorAttributes

  require OpenTelemetry.Tracer, as: Tracer

  @grpc_client_tracer_id OpentelemetryGrpc.Client

  @client_events [
    [:grpc, :client, :rpc, :start],
    [:grpc, :client, :rpc, :stop],
    [:grpc, :client, :rpc, :exception]
  ]

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

  @doc """
  Set up telemetry handlers for gRPC client events.

  ## Examples

      # Basic setup
      OpentelemetryGrpc.Client.setup()

  """
  @spec setup(keyword()) :: :ok
  def setup(opts \\ []) do
    config =
      opts
      |> Enum.into(%{})

    :telemetry.attach_many(
      {__MODULE__, :grpc_client_handler},
      @client_events,
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event([:grpc, :client, :rpc, :start], _measurements, metadata, _config) do
    span_name = "#{metadata.stream.service_name}/#{metadata.stream.method_name}"

    attributes =
      %{
        RPCAttributes.rpc_system() => RPCAttributes.rpc_system_values().grpc,
        RPCAttributes.rpc_service() => metadata.stream.service_name,
        RPCAttributes.rpc_method() => metadata.stream.method_name,
        NetworkAttributes.network_protocol_name() => "http",
        NetworkAttributes.network_protocol_version() => "2",
        NetworkAttributes.network_transport() => "tcp"
      }
      |> maybe_add_server_address(metadata.stream)
      |> maybe_add_server_port(metadata.stream)

    OpentelemetryTelemetry.start_telemetry_span(@grpc_client_tracer_id, span_name, metadata, %{
      kind: :client,
      attributes: attributes
    })
  end

  @doc false
  def handle_event([:grpc, :client, :rpc, :stop], _measurements, metadata, _config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@grpc_client_tracer_id, metadata)

    record_client_span_result(metadata.result)

    OpentelemetryTelemetry.end_telemetry_span(@grpc_client_tracer_id, metadata)
  end

  @doc false
  def handle_event(
        [:grpc, :client, :rpc, :exception],
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = metadata,
        _config
      ) do
    span_ctx = OpentelemetryTelemetry.set_current_telemetry_span(@grpc_client_tracer_id, metadata)

    OpenTelemetry.Span.record_exception(span_ctx, kind, reason, stacktrace)

    OpenTelemetry.Span.set_status(
      span_ctx,
      OpenTelemetry.status(:error, Exception.format_banner(kind, reason, stacktrace))
    )

    OpentelemetryTelemetry.end_telemetry_span(@grpc_client_tracer_id, metadata)
  end

  defp record_client_span_result({:ok, response, _metadata}) do
    # The 3-tuple format returned when return_headers: true option is used.
    record_client_span_result({:ok, response})
  end

  defp record_client_span_result({:ok, _response}) do
    Tracer.set_attributes(%{
      RPCAttributes.rpc_grpc_status_code() => @status_codes.ok
    })
  end

  defp record_client_span_result({:error, %GRPC.RPCError{status: status, message: message}}) do
    Tracer.set_attributes(%{
      RPCAttributes.rpc_grpc_status_code() => Map.get(@status_code_mapping, status, status),
      ErrorAttributes.error_type() => "grpc_error"
    })

    status_name = GRPC.Status.code_name(status)
    Tracer.set_status(OpenTelemetry.status(:error, message || "gRPC error: #{status_name}"))
  end

  defp record_client_span_result({:error, error}) do
    Tracer.set_status(:error, inspect(error))
  end

  defp record_client_span_result(_result), do: :ok

  defp maybe_add_server_address(attrs, %{channel: %{host: host}})
       when is_binary(host) and host != "unknown" do
    Map.put(attrs, ServerAttributes.server_address(), host)
  end

  defp maybe_add_server_address(attrs, _), do: attrs

  defp maybe_add_server_port(attrs, %{channel: %{port: port}})
       when is_integer(port) and port > 0 do
    Map.put(attrs, ServerAttributes.server_port(), port)
  end

  defp maybe_add_server_port(attrs, _), do: attrs
end
