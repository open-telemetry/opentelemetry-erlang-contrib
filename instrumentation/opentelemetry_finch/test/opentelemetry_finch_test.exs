defmodule OpentelemetryFinchTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryFinch

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes

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

    attrs = :otel_attributes.map(attributes)

    expected_attrs = [
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {HTTPAttributes.http_request_method(), "GET"},
      {URLAttributes.url_full(), url},
      {HTTPAttributes.http_response_status_code(), 200}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(attrs, attr) == val, " expected #{attr} to equal #{val}"
    end
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
        HTTPAttributes.http_request_body_size(),
        HTTPAttributes.http_response_body_size(),
        NetworkAttributes.network_transport(),
        URLAttributes.url_scheme(),
        URLAttributes.url_template(),
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

    attrs = :otel_attributes.map(attributes)

    expected_attrs = [
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {HTTPAttributes.http_request_method(), "GET"},
      {URLAttributes.url_full(), endpoint_url(bypass.port)},
      {HTTPAttributes.http_response_status_code(), 200},
      {HTTPAttributes.http_request_body_size(), 0},
      {HTTPAttributes.http_response_body_size(), 0},
      {NetworkAttributes.network_transport(), :tcp},
      {URLAttributes.url_scheme(), :http},
      {URLAttributes.url_template(), "/users/:user_id"},
      {UserAgentAttributes.user_agent_original(), ""}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(attrs, attr) == val, " expected #{attr} to equal #{val}"
    end
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

    attrs = :otel_attributes.map(attributes)

    expected_attrs = [
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {HTTPAttributes.http_request_method(), "GET"},
      {URLAttributes.url_full(), endpoint_url(bypass.port)},
      {HTTPAttributes.http_response_status_code(), 200},
      {String.to_atom("#{HTTPAttributes.http_response_header()}.content-length"), ["0"]},
      {String.to_atom("#{HTTPAttributes.http_response_header()}.server"), ["Cowboy"]},
      {String.to_atom("#{HTTPAttributes.http_request_header()}.authorization"), ["Bearer token"]}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(attrs, attr) == val, " expected #{attr} to equal #{val}"
    end
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

    attrs = :otel_attributes.map(attributes)

    expected_attrs = [
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), 3333},
      {HTTPAttributes.http_request_method(), "GET"},
      {URLAttributes.url_full(), "http://localhost:3333/"},
      {ErrorAttributes.error_type(), "connection refused"}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(attrs, attr) == val, " expected #{attr} to equal #{val}"
    end
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

    attrs = :otel_attributes.map(attributes)

    expected_attrs = [
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {HTTPAttributes.http_request_method(), "GET"},
      {URLAttributes.url_full(), endpoint_url(bypass.port)},
      {HTTPAttributes.http_response_status_code(), 404},
      {ErrorAttributes.error_type(), "404"}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(attrs, attr) == val, " expected #{attr} to equal #{val}"
    end
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
