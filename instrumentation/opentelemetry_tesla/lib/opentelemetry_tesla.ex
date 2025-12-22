defmodule OpentelemetryTesla do
  require OpenTelemetry.SemanticConventions.Trace, as: Trace

  alias OpenTelemetry.Span
  alias OpenTelemetry.Tracer

  @tracer_id __MODULE__

  def setup(_opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :request_start},
      [:tesla, :request, :start],
      &__MODULE__.handle_request_start/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :request_stop},
      [:tesla, :request, :stop],
      &__MODULE__.handle_request_stop/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :request_exception},
      [:tesla, :request, :exception],
      &__MODULE__.handle_request_exception/4,
      %{}
    )
  end

  @doc false
  def handle_request_start(_event, measurements, metadata, config) do
    url = Tesla.build_url(metadata.env.url, metadata.env.query)
    uri = URI.parse(url)
    method = String.upcase(Atom.to_string(metadata.env.method))

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, "HTTP #{method}", metadata, %{
      start_time: measurements.system_time,
      kind: :client,
      attributes: %{
        Trace.http_method() => method,
        Trace.http_scheme() => uri.scheme,
        Trace.net_host_name() => uri.host,
        Trace.net_peer_name() => uri.host,
        Trace.net_peer_port() => uri.port,
        Trace.http_target() => uri.path,
        Trace.http_retry_count() => metadata.env.opts[:retry_count],
        Trace.http_url() => sanitize_url(uri),
        Trace.http_request_content_length() => get_content_length(metadata.env)
      }
    })
  end

  @doc false
  def handle_request_stop(_event, measurements, metadata, _config) do
    OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    status = if metadata.env.status >= 400, do: :error, else: :ok

    Tracer.set_status(status)

    Tracer.set_attributes(%{
      :duration => measurements.duration,
      Trace.http_status_code() => metadata.env.status,
      Trace.http_response_content_length() => get_content_length(metadata.env)
    })

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  @doc false
  def handle_request_exception(_event, measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)
    status = OpenTelemetry.status(:error, inspect(metadata.reason))

    Tracer.set_status(status)

    Span.record_exception(
      ctx,
      metadata.kind,
      metadata.stacktrace,
      %{duration: measurements.duration}
    )

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp sanitize_url(uri) do
    %{uri | userinfo: nil}
    |> URI.to_string()
  end

  defp get_content_length(env) do
    case Enum.find(env.headers, fn {k, _v} -> k == "content-length" end) do
      nil ->
        body_byte_size(env.body)

      {_key, value} ->
        value
    end
  end

  defp body_byte_size(nil), do: 0
  defp body_byte_size(body) when is_binary(body), do: byte_size(body)
  defp body_byte_size(_body), do: nil
end
