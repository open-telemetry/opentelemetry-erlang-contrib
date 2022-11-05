defmodule OpentelemetryFinchTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryFinch

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)

    {:ok, bypass: Bypass.open()}
  end

  test "records span on requests", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)
    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: HttpFinch})

    {:ok, _} = Finch.build(:get, endpoint_url(bypass.port)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                      span(
                        name: "HTTP GET",
                        kind: :client,
                        attributes: attributes
                      )}

    assert %{
      "net.peer.name": "localhost",
      "http.method": "GET",
      "http.target": "/",
      "http.scheme": :http,
      "http.status_code": 200
    } = :otel_attributes.map(attributes)
  end

  test "records span on requests failed", %{bypass: _} do
    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: HttpFinch})

    {:error, _} = Finch.build(:get, endpoint_url(3333)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                      span(
                        name: "HTTP GET",
                        kind: :client,
                        status: {:status, :error, "connection refused"},
                        attributes: attributes
                      )}

    assert %{
      "net.peer.name": "localhost",
      "http.method": "GET",
      "http.target": "/",
      "http.scheme": :http,
      "http.status_code": 0
    } = :otel_attributes.map(attributes)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
