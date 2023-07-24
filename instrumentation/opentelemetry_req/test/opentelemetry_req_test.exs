defmodule OpentelemetryReqTest do
  use ExUnit.Case
  doctest OpentelemetryReq

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    bypass = Bypass.open()

    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    {:ok, bypass: bypass}
  end

  test "Request response headers are extracted as attributes as configured", %{bypass: bypass} do
    client =
      OpentelemetryReq.attach(Req.new(),
        base_url: "http://localhost:#{bypass.port}",
        no_path_params: true,
        req_headers_to_span_attributes: ["Req-Header"],
        resp_headers_to_span_attributes: ["Resp-Header"]
      )

    Bypass.expect_once(bypass, "GET", "/api/users", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("resp-header", "1")
      |> Plug.Conn.resp(204, "")
    end)

    Req.get(client, url: "/api/users", headers: [{"req-header", "1"}])

    assert_receive {:span, span(name: "GET", attributes: attributes)}

    assert %{
             "http.method": "GET",
             "http.url": "http://localhost:#{bypass.port}/api/users",
             "http.target": "/api/users",
             "net.host.name": "localhost",
             "http.scheme": "http",
             "http.status_code": 204,
             "http.request.header.req_header": ["1"],
             "http.response.header.resp_header": ["1"]
           } == :otel_attributes.map(attributes)
  end
end
