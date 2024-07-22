defmodule OpentelemetryBandit do
  @moduledoc """
  OpentelemetryBandit uses [telemetry](https://hexdocs.pm/telemetry/) handlers to create `OpenTelemetry` spans.

  Supported:
  1. :bandit, :request, :start
  2. :bandit, :request, :stop
  3. :bandit, :request, :exception
  4. :bandit, :websocket, :start
  5. :bandit, :websocket, :stop
  """

  alias OpenTelemetry.SemanticConventions.Trace
  require Trace
  require OpenTelemetry.Tracer

  @tracer_id __MODULE__

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(any) :: :ok
  def setup(_opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :request_start},
      [:bandit, :request, :start],
      &__MODULE__.handle_request_start/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :request_stop},
      [:bandit, :request, :stop],
      &__MODULE__.handle_request_stop/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :request_exception},
      [:bandit, :request, :exception],
      &__MODULE__.handle_request_exception/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :websocket_start},
      [:bandit, :websocket, :start],
      &__MODULE__.handle_websocket_start/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :websocket_stop},
      [:bandit, :websocket, :stop],
      &__MODULE__.handle_websocket_stop/4,
      %{}
    )
  end

  def handle_request_start(_event, measurements, meta, _config) do
    conn = Map.get(meta, :conn)

    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration
    resp_body_bytes = Map.get(measurements, :resp_body_bytes, 0)

    url = extract_url(meta, conn)
    request_path = extract_request_path(meta, conn)

    attributes =
      if Map.has_key?(meta, :error) do
        %{
          Trace.http_url() => url,
          Trace.http_method() => conn.method,
          Trace.net_transport() => :"IP.TCP",
          Trace.http_response_content_length() => resp_body_bytes,
          Trace.http_status_code() => conn.status
        }
      else
        %{
          Trace.http_url() => url,
          Trace.http_client_ip() => client_ip(conn),
          Trace.http_scheme() => conn.scheme,
          Trace.net_peer_name() => conn.host,
          Trace.net_peer_port() => conn.port,
          Trace.http_target() => conn.request_path,
          Trace.http_method() => conn.method,

          Trace.http_status_code() => conn.status,
          Trace.http_response_content_length() => resp_body_bytes,

          Trace.net_transport() => :"IP.TCP",
          Trace.http_user_agent() => user_agent(conn)
        }
      end

    span_kind = if Map.has_key?(meta, :error), do: :error, else: :server

    span_id = "HTTP #{conn.method} #{request_path}" |> String.trim()

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_id, meta, %{
      attributes: attributes,
      start_time: measurements.monotonic_time,
      kind: span_kind
    })
  end

  def handle_request_stop(_event, measurements, meta, _config) do
    conn = Map.get(meta, :conn)

    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    attributes =
      if Map.has_key?(meta, :error) do
        %{}
      else
        %{
          Trace.http_status_code() => Map.get(conn, :status),
          Trace.http_response_content_length() => Map.get(measurements, :resp_body_bytes, 0)
        }
      end

    attributes = Map.put(attributes, :duration, measurements.duration)

    OpenTelemetry.Tracer.set_attributes(attributes)

    if Map.get(attributes, Trace.http_status_code()) >= 500 do
      OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error))
    end

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_request_exception(_event, _measurements, meta, _config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error))
    OpenTelemetry.Tracer.record_exception(meta.exception, meta.stacktrace)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  def handle_websocket_start(_event, measurements, meta, _config) do
    attributes = %{
      Trace.net_transport() => :websocket
    }

    span_kind = if Map.has_key?(meta, :error), do: :error, else: :server

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, "Websocket", meta, %{
      attributes: attributes,
      start_time: measurements.monotonic_time,
      kind: span_kind
    })
  end

  def handle_websocket_stop(_event, measurements, meta, _config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

    attributes = %{
      :"websocket.recv.binary.frame.bytes" => Map.get(measurements, :send_binary_frame_bytes, 0),
      :"websocket.send.binary.frame.bytes" => Map.get(measurements, :recv_binary_frame_bytes, 0),
      :duration => measurements.duration
    }

    OpenTelemetry.Tracer.set_attributes(attributes)

    if Map.has_key?(meta, :error) do
      OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, meta.error))
    end

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
  end

  defp extract_url(%{error: _}, conn) do
    case Map.get(conn, :request_target) do
      nil -> ""
      {scheme, host, port, path} -> build_url(scheme, host, port, path)
    end
  end

  defp extract_url(_meta, conn) do
    build_url(conn.scheme, conn.host, conn.port, conn.request_path)
  end

  defp extract_request_path(%{error: _}, conn) do
    case Map.get(conn, :request_target) do
      nil -> ""
      {_, _, _, path} -> path || ""
    end
  end

  defp extract_request_path(_meta, conn) do
    conn.request_path
  end

  defp build_url(scheme, host, port, path), do: "#{scheme}://#{host}:#{port}#{path}"

  defp user_agent(conn) do
    case Plug.Conn.get_req_header(conn, "user-agent") do
      [] -> ""
      [head | _] -> head
    end
  end

  defp client_ip(%{remote_ip: remote_ip} = conn) do
    case Plug.Conn.get_req_header(conn, "x-forwarded-for") do
      [] ->
        remote_ip
        |> :inet_parse.ntoa()
        |> to_string()

      [ip_address | _] ->
        ip_address
    end
  end
end
