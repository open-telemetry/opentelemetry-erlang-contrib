defmodule Tesla.Middleware.OpenTelemetryTest do
  use ExUnit.Case
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
    bypass = Bypass.open()

    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    {:ok, bypass: bypass, base_url: endpoint_url(bypass.port)}
  end

  describe "span name" do
    test "uses generic route name when opentelemetry middleware is configured before path params middleware",
         %{
           bypass: bypass,
           base_url: base_url
         } do
      Bypass.expect_once(bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, base_url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET /users/:id", attributes: _attributes)}
    end

    test "uses low-cardinality method name when path params middleware is not used",
         %{
           bypass: bypass,
           base_url: base_url
         } do
      Bypass.expect_once(bypass, "GET", "/users/", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, base_url},
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/")

      assert_receive {:span, span(name: "GET", attributes: _attributes)}
    end

    test "uses custom span name when passed in middleware opts",
         %{
           bypass: bypass,
           base_url: base_url
         } do
      Bypass.expect_once(bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, base_url},
          {Tesla.Middleware.OpenTelemetry, span_name: "my-high-cardinality-url"},
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET my-high-cardinality-url", attributes: _attributes)}
    end

    test "uses custom span name function when passed in middleware opts",
         %{
           bypass: bypass,
           base_url: base_url
         } do
      Bypass.expect_once(bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, base_url},
          {Tesla.Middleware.OpenTelemetry, span_name: fn _env -> "potato" end},
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET potato", attributes: _attributes)}
    end
  end

  test "Records spans for Tesla HTTP client", %{bypass: bypass, base_url: base_url} do
    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(name: "GET", attributes: _attributes)}
  end

  @error_codes [
    400,
    401,
    402,
    403,
    404,
    405,
    406,
    407,
    408,
    409,
    410,
    411,
    412,
    413,
    414,
    415,
    416,
    417,
    418,
    500,
    501,
    502,
    503,
    504,
    505,
    506,
    507,
    508
  ]

  for code <- @error_codes do
    test "Marks Span status as :error when HTTP request fails with #{code}", %{
      bypass: bypass,
      base_url: base_url
    } do
      Bypass.expect_once(bypass, "GET", "/users", fn conn ->
        Plug.Conn.resp(conn, unquote(code), "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, base_url},
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/")

      assert_receive {:span, span(status: {:status, :error, ""})}
    end
  end

  test "Marks Span status as :errors when max redirects are exceeded", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.expect(bypass, "GET", "/users", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("Location", "/users/1")
      |> Plug.Conn.resp(301, "")
    end)

    Bypass.expect(bypass, "GET", "/users/1", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("Location", "/users/2")
      |> Plug.Conn.resp(301, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry,
        {Tesla.Middleware.FollowRedirects, max_redirects: 1}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, _}, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[ErrorAttributes.error_type()] == Tesla.Middleware.FollowRedirects
  end

  test "Marks Span status as :ok if error status is within `mark_status_ok` opt list",
       %{bypass: bypass, base_url: base_url} do
    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 404, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        {Tesla.Middleware.OpenTelemetry, mark_status_ok: [404]}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :ok, ""})}
  end

  test "Marks Span status as :error unless error status is within `mark_status_ok` opt list",
       %{bypass: bypass, base_url: base_url} do
    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 404, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        {Tesla.Middleware.OpenTelemetry, mark_status_ok: []}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, ""})}
  end

  test "Appends query string parameters to url.full attribute", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token", array: ["foo", "bar"]]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{bypass.port}/users/2?token=some-token&array%5B%5D=foo&array%5B%5D=bar"
  end

  test "url.full attribute is correct when request doesn't contain query string parameters", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, []}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{bypass.port}/users/2"
  end

  test "Handles url path arguments correctly", %{bypass: bypass, base_url: base_url} do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token"]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{bypass.port}/users/2?token=some-token"
  end

  test "Records http.response.body.size param into the span", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO 👋")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        {Tesla.Middleware.OpenTelemetry,
         opt_in_attrs: [HTTPAttributes.http_response_body_size()]},
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token"]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    {response_size, _} =
      Integer.parse(mapped_attributes[HTTPAttributes.http_response_body_size()])

    assert response_size == byte_size("HELLO 👋")
  end

  test "Marks Span status as :error when adapter returns {:error, _}", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.down(bypass)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, _}, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    expected_attrs = [
      {ErrorAttributes.error_type(), ""},
      {HTTPAttributes.http_request_method(), "GET"},
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(mapped_attributes, attr) == val, " expected #{attr} to equal #{val}"
    end
  end

  test "Adds opt-in attributes only from opt_in_attrs list", %{bypass: bypass, base_url: base_url} do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO")
    end)

    otel_opts = [
      opt_in_attrs: [
        HTTPAttributes.http_request_body_size(),
        HTTPAttributes.http_response_body_size(),
        NetworkAttributes.network_transport(),
        URLAttributes.url_scheme(),
        URLAttributes.url_template(),
        UserAgentAttributes.user_agent_original()
      ]
    ]

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        {Tesla.Middleware.OpenTelemetry, otel_opts},
        Tesla.Middleware.PathParams
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    expected_attrs = [
      {HTTPAttributes.http_request_method(), "GET"},
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {URLAttributes.url_full(), "http://localhost:#{bypass.port}/users/2"},
      {HTTPAttributes.http_response_status_code(), 200},
      {URLAttributes.url_scheme(), "http"},
      {URLAttributes.url_template(), "/users/:id"},
      {HTTPAttributes.http_request_body_size(), "0"},
      {HTTPAttributes.http_response_body_size(), "5"},
      {NetworkAttributes.network_transport(), :tcp},
      {UserAgentAttributes.user_agent_original(), ""}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(mapped_attributes, attr) == val, " expected #{attr} to equal #{val}"
    end
  end

  test "Adds request and response headers only from request_header_attrs and response_header_attrs lists",
       %{
         bypass: bypass,
         base_url: base_url
       } do
    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO")
    end)

    otel_opts = [
      request_header_attrs: ["authorization"],
      response_header_attrs: ["content-length", "server"]
    ]

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, base_url},
        {Tesla.Middleware.Headers, [{"authorization", "Bearer token"}]},
        {Tesla.Middleware.OpenTelemetry, otel_opts}
      ])

    Tesla.get(client, "/users")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    expected_attrs = [
      {HTTPAttributes.http_request_method(), "GET"},
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {URLAttributes.url_full(), "http://localhost:#{bypass.port}/users"},
      {HTTPAttributes.http_response_status_code(), 200},
      {String.to_atom("#{HTTPAttributes.http_response_header()}.content-length"), ["5"]},
      {String.to_atom("#{HTTPAttributes.http_response_header()}.server"), ["Cowboy"]},
      {String.to_atom("#{HTTPAttributes.http_request_header()}.authorization"), ["Bearer token"]}
    ]

    for {attr, val} <- expected_attrs do
      assert Map.get(mapped_attributes, attr) == val, " expected #{attr} to equal #{val}"
    end
  end

  describe "trace propagation" do
    test "injects distributed tracing headers by default" do
      {:ok, env} = Tesla.get(client(), "/propagate-traces")

      assert traceparent = Tesla.get_header(env, "traceparent")
      assert is_binary(traceparent)

      assert_receive {:span, span(name: _name, attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)

      assert mapped_attributes[URLAttributes.url_full()] == "/propagate-traces"
    end

    test "optionally disable propagation but keep span report" do
      {:ok, env} = Tesla.get(client(propagator: :none), "/propagate-traces")

      refute Tesla.get_header(env, "traceparent")

      assert_receive {:span, span(name: _name, attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)

      assert mapped_attributes[URLAttributes.url_full()] == "/propagate-traces"
    end
  end

  defp client(opts \\ []) do
    [{Tesla.Middleware.OpenTelemetry, opts}]
    |> Tesla.client(fn env -> {:ok, env} end)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
