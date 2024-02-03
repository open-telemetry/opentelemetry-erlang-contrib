defmodule OpentelemetryBandit do
  @moduledoc """
  OpentelemetryBandit uses [telemetry](https://hexdocs.pm/telemetry/) handlers to create `OpenTelemetry` spans.

  Supported:
  1. :bandit, :request, :stop
  2. :bandit, :request, :exception
  3. :bandit, :websocket, :stop
  """

  alias OpenTelemetry.SemanticConventions.Trace
  require Trace
  require OpenTelemetry.Tracer

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(any) :: :ok
  def setup(_opts \\ []) do
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
      {__MODULE__, :websocket_stop},
      [:bandit, :websocket, :stop],
      &__MODULE__.handle_websocket_stop/4,
      %{}
    )
  end

  def handle_request_stop(_event, measurements, meta, _config) do
    conn = Map.get(meta, :conn)
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    url = extract_url(meta, conn)
    request_path = extract_request_path(meta, conn)

    attributes =
      if Map.has_key?(meta, :error) do
        %{
          Trace.http_url() => url,
          Trace.http_method() => meta.method,
          Trace.net_transport() => :"IP.TCP",
          Trace.http_response_content_length() => measurements.resp_body_bytes,
          Trace.http_status_code() => meta.status
        }
      else
        %{
          Trace.http_url() => url,
          Trace.http_client_ip() => client_ip(conn),
          Trace.http_scheme() => conn.scheme,
          Trace.net_peer_name() => conn.host,
          Trace.net_peer_port() => conn.port,
          Trace.http_target() => conn.request_path,
          Trace.http_method() => meta.method,
          Trace.http_status_code() => meta.status,
          Trace.http_response_content_length() => measurements.resp_body_bytes,
          Trace.net_transport() => :"IP.TCP",
          Trace.http_user_agent() => user_agent(conn)
        }
      end

    span_kind = if Map.has_key?(meta, :error), do: :error, else: :server

    span_id = "HTTP #{meta.method} #{request_path}" |> String.trim()

    OpenTelemetry.Tracer.start_span(span_id, %{
      attributes: attributes,
      start_time: start_time,
      end_time: end_time,
      kind: span_kind
    })
    |> set_span_status(meta, Map.get(meta, :error, ""))
    |> OpenTelemetry.Span.end_span()

    OpenTelemetry.Ctx.clear()
  end

  def handle_request_exception(_event, _measurements, meta, _config) do
    OpenTelemetry.Tracer.start_span("HTTP exception #{inspect(meta.exception.__struct__)}", %{
      kind: :error,
      status: :error
    })
    |> set_span_status(meta, inspect(meta.stacktrace))
    |> OpenTelemetry.Span.end_span()

    OpenTelemetry.Ctx.clear()
  end

  def handle_websocket_stop(_event, measurements, meta, _config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    attributes = %{
      :"websocket.recv.binary.frame.bytes" => Map.get(measurements, :send_binary_frame_bytes, 0),
      :"websocket.send.binary.frame.bytes" => Map.get(measurements, :recv_binary_frame_bytes, 0),
      Trace.net_transport() => :websocket
    }

    span_kind = if Map.has_key?(meta, :error), do: :error, else: :server

    OpenTelemetry.Tracer.start_span("Websocket", %{
      attributes: attributes,
      start_time: start_time,
      end_time: end_time,
      kind: span_kind
    })
    |> set_span_status(meta, Map.get(meta, :error, ""))
    |> OpenTelemetry.Span.end_span()

    OpenTelemetry.Ctx.clear()
  end

  defp set_span_status(span, meta, message) do
    status = if Map.has_key?(meta, :error) || message != "", do: :error, else: :ok

    OpenTelemetry.Span.set_status(span, OpenTelemetry.status(status, message))
    span
  end

  defp extract_url(%{error: _} = meta, _conn) do
    case Map.get(meta, :request_target) do
      nil -> ""
      {scheme, host, port, path} -> build_url(scheme, host, port, path)
    end
  end

  defp extract_url(_meta, conn) do
    build_url(conn.scheme, conn.host, conn.port, conn.request_path)
  end

  defp extract_request_path(%{error: _} = meta, _conn) do
    case Map.get(meta, :request_target) do
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
