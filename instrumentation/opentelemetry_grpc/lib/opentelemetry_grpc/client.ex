defmodule OpentelemetryGrpc.Client do
  @moduledoc """
  OpenTelemetry handler for gRPC client telemetry events.

  Attaches telemetry handlers for gRPC client events, automatically creating
  OpenTelemetry spans for outgoing RPC calls following OpenTelemetry semantic
  conventions.
  """

  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.ErrorAttributes

  alias OpenTelemetry.Tracer
  alias OpentelemetryGrpc.SemConv
  alias OpentelemetryGrpc.StatusCode

  @grpc_client_tracer_id OpentelemetryGrpc.Client

  @client_events [
    [:grpc, :client, :rpc, :start],
    [:grpc, :client, :rpc, :stop],
    [:grpc, :client, :rpc, :exception]
  ]

  @status_code_ok GRPC.Status.ok()

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
      |> Map.put(:stability, SemConv.stability())

    :telemetry.attach_many(
      {__MODULE__, :grpc_client_handler},
      @client_events,
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event([:grpc, :client, :rpc, :start], _measurements, metadata, config) do
    span_name = "#{metadata.stream.service_name}/#{metadata.stream.method_name}"

    attributes =
      config.stability
      |> SemConv.rpc_attributes(metadata.stream.service_name, metadata.stream.method_name)
      |> Map.merge(%{
        NetworkAttributes.network_protocol_name() => "http",
        NetworkAttributes.network_protocol_version() => "2"
      })
      |> maybe_add_server_address(metadata.stream)
      |> maybe_add_server_port(metadata.stream)

    OpentelemetryTelemetry.start_telemetry_span(@grpc_client_tracer_id, span_name, metadata, %{
      kind: :client,
      attributes: attributes
    })
  end

  @doc false
  def handle_event([:grpc, :client, :rpc, :stop], _measurements, metadata, config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@grpc_client_tracer_id, metadata)

    record_client_span_result(metadata.result, config.stability)

    OpentelemetryTelemetry.end_telemetry_span(@grpc_client_tracer_id, metadata)
  end

  @doc false
  def handle_event([:grpc, :client, :rpc, :exception], _measurements, metadata, _config) do
    span_ctx = OpentelemetryTelemetry.set_current_telemetry_span(@grpc_client_tracer_id, metadata)
    record_client_exception(span_ctx, metadata)
    OpentelemetryTelemetry.end_telemetry_span(@grpc_client_tracer_id, metadata)
  end

  # the call failed before a status code came back, so the exception names the error
  defp record_client_exception(span_ctx, %{reason: %error_struct{} = reason} = metadata)
       when is_exception(reason) do
    OpenTelemetry.Span.record_exception(span_ctx, reason, metadata.stacktrace)

    OpenTelemetry.Span.set_attributes(span_ctx, %{
      ErrorAttributes.error_type() => to_string(error_struct)
    })

    OpenTelemetry.Span.set_status(
      span_ctx,
      OpenTelemetry.status(:error, Exception.message(reason))
    )
  end

  # throw and exit carry no exception to record or name the error by
  defp record_client_exception(span_ctx, metadata) do
    OpenTelemetry.Span.set_attributes(span_ctx, %{ErrorAttributes.error_type() => "_OTHER"})

    OpenTelemetry.Span.set_status(
      span_ctx,
      OpenTelemetry.status(
        :error,
        Exception.format_banner(metadata.kind, metadata.reason, metadata.stacktrace)
      )
    )
  end

  defp record_client_span_result({:ok, response, _metadata}, stability) do
    # The 3-tuple format returned when return_headers: true option is used.
    record_client_span_result({:ok, response}, stability)
  end

  defp record_client_span_result({:ok, _response}, stability) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, @status_code_ok))
  end

  defp record_client_span_result(
         {:error, %GRPC.RPCError{status: status, message: message}},
         stability
       ) do
    Tracer.set_attributes(SemConv.status_code_attributes(stability, status))
    Tracer.set_attributes(%{ErrorAttributes.error_type() => StatusCode.name(status)})

    status_name = GRPC.Status.code_name(status)
    Tracer.set_status(OpenTelemetry.status(:error, message || "gRPC error: #{status_name}"))
  end

  # no status code came back, so no status name can describe the error
  defp record_client_span_result({:error, error}, _stability) do
    Tracer.set_attributes(%{ErrorAttributes.error_type() => "_OTHER"})
    Tracer.set_status(:error, inspect(error))
  end

  defp record_client_span_result(_result, _stability), do: :ok

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
