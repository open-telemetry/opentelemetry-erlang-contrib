defmodule OpentelemetryFinch do
  @moduledoc """
  OpentelemetryFinch uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
  create `OpenTelemetry` spans.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryFinch.setup()

        # ...
      end

  """

  alias OpenTelemetry.SemanticConventions.Trace

  require Trace
  require OpenTelemetry.Tracer

  @typedoc "Setup options"
  @type opts :: [
          {:req_headers_to_span_attributes, String.t()}
          | {:resp_headers_to_span_attributes, String.t()}
        ]

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts()) :: :ok
  def setup(opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :request_stop},
      [:finch, :request, :stop],
      &__MODULE__.handle_request_stop/4,
      parse_opts(opts)
    )
  end

  @doc false
  def handle_request_stop(_event, measurements, meta, config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    {response, status} =
      case meta.result do
        {:ok, response} -> {response, response.status}
        _ -> {%{}, 0}
      end

    url = build_url(meta.request.scheme, meta.request.host, meta.request.port, meta.request.path)

    attributes = %{
      Trace.http_url() => url,
      Trace.http_scheme() => meta.request.scheme,
      Trace.net_peer_name() => meta.request.host,
      Trace.net_peer_port() => meta.request.port,
      Trace.http_target() => meta.request.path,
      Trace.http_method() => meta.request.method,
      Trace.http_status_code() => status
    }

    req_headers_attributes =
      :opentelemetry_instrumentation_http.extract_headers_attributes(
        :request,
        meta.request.headers,
        Map.get(config, :req_headers_to_span_attributes, [])
      )

    resp_headers_attributes =
      :opentelemetry_instrumentation_http.extract_headers_attributes(
        :response,
        Map.get(response, :headers, []),
        Map.get(config, :resp_headers_to_span_attributes, [])
      )

    attributes =
      attributes
      |> Map.merge(req_headers_attributes)
      |> Map.merge(resp_headers_attributes)

    s =
      OpenTelemetry.Tracer.start_span("HTTP #{meta.request.method}", %{
        start_time: start_time,
        attributes: attributes,
        kind: :client
      })

    if meta.result |> elem(0) == :error do
      OpenTelemetry.Span.set_status(
        s,
        OpenTelemetry.status(:error, format_error(meta.result |> elem(1)))
      )
    end

    OpenTelemetry.Span.end_span(s)
  end

  defp build_url(scheme, host, port, path), do: "#{scheme}://#{host}:#{port}#{path}"

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)

  defp parse_opts(opts) do
    opts
    |> Map.new()
    |> Map.update(:req_headers_to_span_attributes, [], &parse_headers_to_span_attributes/1)
    |> Map.update(:resp_headers_to_span_attributes, [], &parse_headers_to_span_attributes/1)
  end

  defp parse_headers_to_span_attributes(headers) do
    Enum.map(headers, &:opentelemetry_instrumentation_http.normalize_header_name/1)
  end
end
