defmodule OpentelemetryFinchTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryFinch

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.{NetworkAttributes, UserAgentAttributes, Incubating}

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    OpentelemetryFinch.setup()

    _conn = start_supervised!({Finch, name: HttpFinch})

    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)

    {:ok, bypass: Bypass.open()}
  end

  test "records span on requests", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    url = "#{endpoint_url(bypass.port)}/users/2?token=some-token&array=foo&array=bar"
    {:ok, _} = Finch.build(:get, url) |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "GET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "server.address": "localhost",
             "server.port": bypass.port,
             "http.request.method": "GET",
             "url.full": url,
             "http.response.status_code": 200
           } == :otel_attributes.map(attributes)
  end

  test "records span on requests with custom span name", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    otel_config = %{span_name: "/users/:user_id"}

    {:ok, _} =
      Finch.build(:get, endpoint_url(bypass.port))
      |> Finch.Request.put_private(:otel, otel_config)
      |> Finch.request(HttpFinch)

    assert_receive {:span, span(name: "GET /users/:user_id", kind: :client)}
  end

  test "records span on requests with url template", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    otel_config = %{url_template: "/users/:user_id"}

    {:ok, _} =
      Finch.build(:get, endpoint_url(bypass.port))
      |> Finch.Request.put_private(:otel, otel_config)
      |> Finch.request(HttpFinch)

    assert_receive {:span, span(name: "GET /users/:user_id", kind: :client)}
  end

  test "adds opt-in attrs to span when opt_in_attrs is set", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    otel_config = %{
      opt_in_attrs: [
        Incubating.HTTPAttributes.http_request_body_size(),
        Incubating.HTTPAttributes.http_response_body_size(),
        NetworkAttributes.network_transport(),
        Incubating.URLAttributes.url_scheme(),
        Incubating.URLAttributes.url_template(),
        UserAgentAttributes.user_agent_original()
      ],
      url_template: "/users/:user_id"
    }

    {:ok, _} =
      Finch.build(:get, endpoint_url(bypass.port))
      |> Finch.Request.put_private(:otel, otel_config)
      |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "GET /users/:user_id",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "server.address": "localhost",
             "server.port": bypass.port,
             "http.request.method": "GET",
             "url.full": endpoint_url(bypass.port),
             "http.response.status_code": 200,
             "http.request.body.size": 0,
             "http.response.body.size": 0,
             "network.transport": :tcp,
             "url.scheme": :http,
             "url.template": "/users/:user_id",
             "user_agent.original": ""
           } == :otel_attributes.map(attributes)
  end

  test "adds request and response headers to span when request_header_attrs and response_header_attrs are set",
       %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    otel_config = %{
      request_header_attrs: ["authorization"],
      response_header_attrs: ["content-length", "server"]
    }

    {:ok, _} =
      Finch.build(:get, endpoint_url(bypass.port), [{"authorization", "Bearer token"}])
      |> Finch.Request.put_private(:otel, otel_config)
      |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "GET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "server.address": "localhost",
             "server.port": bypass.port,
             "http.request.method": "GET",
             "url.full": endpoint_url(bypass.port),
             "http.response.status_code": 200,
             "http.response.header.content-length": ["0"],
             "http.response.header.server": ["Cowboy"],
             "http.request.header.authorization": ["Bearer token"]
           } == :otel_attributes.map(attributes)
  end

  test "records span on requests failed", %{bypass: _} do
    {:error, _} = Finch.build(:get, endpoint_url(3333)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "GET",
                      kind: :client,
                      status: {:status, :error, "connection refused"},
                      attributes: attributes
                    )}

    assert %{
             "server.address": "localhost",
             "server.port": 3333,
             "http.request.method": "GET",
             "url.full": "http://localhost:3333/",
             "error.type": "connection refused"
           } == :otel_attributes.map(attributes)
  end

  test "records span on request response with 4xx status code", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 404, "") end)

    {:ok, _} = Finch.build(:get, endpoint_url(bypass.port)) |> Finch.request(HttpFinch)

    assert_receive {:span,
                    span(
                      name: "GET",
                      kind: :client,
                      status: {:status, :error, "404"},
                      attributes: attributes
                    )}

    assert %{
             "server.address": "localhost",
             "server.port": bypass.port,
             "http.request.method": "GET",
             "url.full": endpoint_url(bypass.port),
             "http.response.status_code": 404,
             "error.type": "404"
           } == :otel_attributes.map(attributes)
  end

  test "logs error and doesn't record span if invalid otel option is passed", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn -> Plug.Conn.resp(conn, 200, "") end)

    log =
      ExUnit.CaptureLog.capture_log(fn ->
        Finch.build(:get, endpoint_url(bypass.port))
        |> Finch.Request.put_private(:otel, %{invalid_option: "invalid_value"})
        |> Finch.request(HttpFinch)
      end)

    assert log =~ """
           Reason=%NimbleOptions.ValidationError{
             message: \"unknown options [:invalid_option], valid options are: [:opt_in_attrs, :request_header_attrs, :response_header_attrs, :span_name, :url_template]\",
             key: [:invalid_option],
             value: nil,
             keys_path: []
           }\
           """

    refute_receive {:span, _}
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
