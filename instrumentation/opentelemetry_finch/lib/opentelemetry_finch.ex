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

  require OpenTelemetry.Tracer

  @typedoc "Setup options"
  @type opts :: []

  @doc """
  Initializes and configures the telemetry handlers.
  """
  @spec setup(opts()) :: :ok
  def setup(_opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :request_stop},
      [:finch, :request, :stop],
      &__MODULE__.handle_request_stop/4,
      %{}
    )
  end

  @doc false
  def handle_request_stop(_event, measurements, meta, _config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    status =
      case meta.result do
        {:ok, response} -> response.status
        _ -> 0
      end

    url = build_url(meta.request.scheme, meta.request.host, meta.request.port, meta.request.path)

    attributes = %{
      "http.url": url,
      "http.scheme": meta.request.scheme,
      "net.peer.name": meta.request.host,
      "net.peer.port": meta.request.port,
      "http.target": meta.request.path,
      "http.method": meta.request.method,
      "http.status_code": status
    }

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
end
