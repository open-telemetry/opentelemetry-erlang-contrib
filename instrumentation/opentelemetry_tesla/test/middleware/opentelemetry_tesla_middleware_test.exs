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

  test "basic GET request records span with correct kind and core attributes", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 200, "ok")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.get(client, "/users")

    assert_receive {:span,
                    span(
                      name: :GET,
                      kind: :client,
                      attributes: attributes
                    )}

    assert :otel_attributes.map(attributes) == %{
             HTTPAttributes.http_request_method() => :GET,
             HTTPAttributes.http_response_status_code() => 200,
             ServerAttributes.server_address() => "localhost",
             ServerAttributes.server_port() => ctx.bypass.port,
             URLAttributes.url_full() => "http://localhost:#{ctx.bypass.port}/users"
           }
  end

  test "POST request uses correct HTTP method attribute", ctx do
    Bypass.expect_once(ctx.bypass, "POST", "/users", fn conn ->
      Plug.Conn.resp(conn, 201, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.post(client, "/users", "body")

    assert_receive {:span, span(name: :POST, kind: :client, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_method()] == :POST
    assert mapped_attributes[HTTPAttributes.http_response_status_code()] == 201
  end

  test "PUT request uses correct HTTP method attribute", ctx do
    Bypass.expect_once(ctx.bypass, "PUT", "/users/1", fn conn ->
      Plug.Conn.resp(conn, 200, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.put(client, "/users/1", "body")

    assert_receive {:span, span(name: :PUT, kind: :client, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_method()] == :PUT
  end

  test "DELETE request uses correct HTTP method attribute", ctx do
    Bypass.expect_once(ctx.bypass, "DELETE", "/users/1", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.delete(client, "/users/1")

    assert_receive {:span, span(name: :DELETE, kind: :client, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_method()] == :DELETE
  end

  describe "span name" do
    test "uses generic route name when opentelemetry middleware is configured with path params and keep request middleware",
         %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, endpoint_url(bypass.port)},
          Tesla.Middleware.KeepRequest,
          Tesla.Middleware.PathParams,
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET /users/:id", attributes: _attributes)}
    end

    test "uses generic route name when opentelemetry middleware is configured before path params middleware",
         ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET /users/:id", attributes: _attributes)}
    end

    test "uses low-cardinality method name when path params middleware is not used",
         ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users/", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/")

      assert_receive {:span, span(name: :GET, attributes: _attributes)}
    end

    test "uses custom span name when passed in middleware opts",
         ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          {Tesla.Middleware.OpenTelemetry, span_name: "my-high-cardinality-url"},
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET my-high-cardinality-url", attributes: _attributes)}
    end

    test "uses template from KeepRequest when OTel is after PathParams",
         ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users/3", fn conn ->
        Plug.Conn.resp(conn, 200, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.KeepRequest,
          Tesla.Middleware.PathParams,
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "GET /users/:id", attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)

      assert mapped_attributes[URLAttributes.url_full()] ==
               "http://localhost:#{ctx.bypass.port}/users/3"
    end

    test "url.full has template on error path in legacy ordering (OTel before PathParams)",
         ctx do
      Bypass.down(ctx.bypass)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span,
                      span(
                        name: "GET /users/:id",
                        status: {:status, :error, _},
                        attributes: attributes
                      )}

      mapped_attributes = :otel_attributes.map(attributes)

      assert mapped_attributes[URLAttributes.url_full()] ==
               "http://localhost:#{ctx.bypass.port}/users/:id"
    end
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
    test "Marks Span status as :error when HTTP request fails with #{code}", ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
        Plug.Conn.resp(conn, unquote(code), "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users/")

      assert_receive {:span, span(status: {:status, :error, ""})}
    end
  end

  test "Marks Span status as :errors when max redirects are exceeded", ctx do
    Bypass.expect(ctx.bypass, "GET", "/users", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("Location", "/users/1")
      |> Plug.Conn.resp(301, "")
    end)

    Bypass.expect(ctx.bypass, "GET", "/users/1", fn conn ->
      conn
      |> Plug.Conn.put_resp_header("Location", "/users/2")
      |> Plug.Conn.resp(301, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry,
        {Tesla.Middleware.FollowRedirects, max_redirects: 1}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, _}, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[ErrorAttributes.error_type()] == Tesla.Middleware.FollowRedirects
  end

  test "Marks Span status as :ok if error status is within `mark_status_ok` opt list",
       ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 404, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry, mark_status_ok: [404]}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :ok, ""})}
  end

  test "Marks Span status as :error unless error status is within `mark_status_ok` opt list",
       ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 404, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry, mark_status_ok: []}
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, ""})}
  end

  test "Appends query string parameters to url.full attribute", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token", array: ["foo", "bar"]]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{ctx.bypass.port}/users/2?token=some-token&array%5B%5D=foo&array%5B%5D=bar"
  end

  test "url.full attribute is correct when request doesn't contain query string parameters",
       ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, []}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{ctx.bypass.port}/users/2"
  end

  test "Handles url path arguments correctly", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry,
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token"]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{ctx.bypass.port}/users/2?token=some-token"
  end

  test "Records http.response.body.size param into the span", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO 👋")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry,
         opt_in_attrs: [HTTPAttributes.http_response_body_size()]},
        Tesla.Middleware.PathParams,
        {Tesla.Middleware.Query, [token: "some-token"]}
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    response_size = Map.get(mapped_attributes, HTTPAttributes.http_response_body_size())

    assert response_size == byte_size("HELLO 👋")
  end

  test "Marks Span status as :error when adapter returns {:error, _}", ctx do
    Bypass.down(ctx.bypass)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.get(client, "/users/")

    assert_receive {:span, span(status: {:status, :error, _}, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[HTTPAttributes.http_request_method()] == :GET
    assert mapped_attributes[ServerAttributes.server_address()] == "localhost"
    assert mapped_attributes[ServerAttributes.server_port()] == ctx.bypass.port

    assert mapped_attributes[URLAttributes.url_full()] ==
             "http://localhost:#{ctx.bypass.port}/users/"

    assert mapped_attributes[ErrorAttributes.error_type()] == :econnrefused
  end

  test "Adds opt-in attributes only from opt_in_attrs list", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
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
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry, otel_opts},
        Tesla.Middleware.PathParams
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    assert :otel_attributes.map(attributes) == %{
             HTTPAttributes.http_request_method() => :GET,
             ServerAttributes.server_address() => "localhost",
             ServerAttributes.server_port() => ctx.bypass.port,
             URLAttributes.url_full() => "http://localhost:#{ctx.bypass.port}/users/2",
             HTTPAttributes.http_response_status_code() => 200,
             URLAttributes.url_scheme() => :http,
             URLAttributes.url_template() => "/users/:id",
             HTTPAttributes.http_response_body_size() => 5,
             NetworkAttributes.network_transport() => :tcp
           }
  end

  test "Adds request and response headers only from request_header_attrs and response_header_attrs lists",
       ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO")
    end)

    otel_opts = [
      request_header_attrs: ["authorization"],
      response_header_attrs: ["content-length", "server"]
    ]

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.Headers, [{"authorization", "Bearer token"}]},
        {Tesla.Middleware.OpenTelemetry, otel_opts}
      ])

    Tesla.get(client, "/users")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    assert :otel_attributes.map(attributes) == %{
             HTTPAttributes.http_request_method() => :GET,
             ServerAttributes.server_address() => "localhost",
             ServerAttributes.server_port() => ctx.bypass.port,
             URLAttributes.url_full() => "http://localhost:#{ctx.bypass.port}/users",
             HTTPAttributes.http_response_status_code() => 200,
             String.to_atom("#{HTTPAttributes.http_response_header()}.content-length") => ["5"],
             String.to_atom("#{HTTPAttributes.http_response_header()}.server") => ["Cowboy"],
             String.to_atom("#{HTTPAttributes.http_request_header()}.authorization") => [
               "Bearer token"
             ]
           }
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
      {:ok, env} =
        Tesla.get(client(propagate_trace_headers: false), "/propagate-traces")

      refute Tesla.get_header(env, "traceparent")

      assert_receive {:span, span(name: _name, attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)

      assert mapped_attributes[URLAttributes.url_full()] == "/propagate-traces"
    end
  end

  test "Adds extra_attrs to span", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 200, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry, extra_attrs: %{:"custom.attribute" => "test-value"}}
      ])

    Tesla.get(client, "/users")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[:"custom.attribute"] == "test-value"
  end

  test "opt-in attributes are absent when not configured", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry, opt_in_attrs: [URLAttributes.url_scheme()]},
        Tesla.Middleware.PathParams
      ])

    Tesla.get(client, "/users/:id", opts: [path_params: [id: "2"]])

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert Map.has_key?(mapped_attributes, URLAttributes.url_scheme())

    refute Map.has_key?(mapped_attributes, HTTPAttributes.http_request_body_size())
    refute Map.has_key?(mapped_attributes, HTTPAttributes.http_response_body_size())
    refute Map.has_key?(mapped_attributes, NetworkAttributes.network_transport())
    refute Map.has_key?(mapped_attributes, URLAttributes.url_template())
    refute Map.has_key?(mapped_attributes, UserAgentAttributes.user_agent_original())
  end

  test "sets error.type to the exception module for struct errors" do
    client =
      Tesla.client(
        [{Tesla.Middleware.OpenTelemetry, []}],
        fn _env -> {:error, %Tesla.Error{reason: :econnrefused}} end
      )

    Tesla.get(client, "/fail")

    assert_receive {:span, span(status: {:status, :error, _}, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[ErrorAttributes.error_type()] == Tesla.Error
  end

  test "extra_attrs do not override instrumented attributes", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 200, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        {Tesla.Middleware.OpenTelemetry,
         extra_attrs: %{HTTPAttributes.http_request_method() => "OVERRIDDEN"}}
      ])

    Tesla.get(client, "/users")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_method()] == :GET
  end

  test "url.full strips credentials from URL", ctx do
    Bypass.expect_once(ctx.bypass, "GET", "/secret", fn conn ->
      Plug.Conn.resp(conn, 200, "")
    end)

    client =
      Tesla.client([
        {Tesla.Middleware.BaseUrl, ctx.base_url},
        Tesla.Middleware.OpenTelemetry
      ])

    Tesla.get(client, "/secret")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    url_full = mapped_attributes[URLAttributes.url_full()]
    refute url_full =~ "user"
    refute url_full =~ "pass"
  end

  describe "server.port defaults" do
    test "defaults to 80 when no port in http URL" do
      {:ok, _env} = Tesla.get(client(), "http://example.com/test")

      assert_receive {:span, span(name: _name, attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)
      assert mapped_attributes[ServerAttributes.server_port()] == 80
    end

    test "uses explicit port from URL", ctx do
      Bypass.expect_once(ctx.bypass, "GET", "/users", fn conn ->
        Plug.Conn.resp(conn, 200, "")
      end)

      client =
        Tesla.client([
          {Tesla.Middleware.BaseUrl, ctx.base_url},
          Tesla.Middleware.OpenTelemetry
        ])

      Tesla.get(client, "/users")

      assert_receive {:span, span(name: _name, attributes: attributes)}

      mapped_attributes = :otel_attributes.map(attributes)
      assert mapped_attributes[ServerAttributes.server_port()] == ctx.bypass.port
    end
  end

  test "sets http.request.resend_count when retry_count is present in env.opts" do
    client =
      Tesla.client(
        [{Tesla.Middleware.OpenTelemetry, []}],
        fn env -> {:ok, %{env | status: 200}} end
      )

    Tesla.request(client, method: :get, url: "http://example.com/test", opts: [retry_count: 2])

    assert_receive {:span, span(name: _, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_resend_count()] == 2
  end

  test "omits http.request.resend_count when retry_count is 0 or absent" do
    client =
      Tesla.client(
        [{Tesla.Middleware.OpenTelemetry, []}],
        fn env -> {:ok, %{env | status: 200}} end
      )

    Tesla.request(client, method: :get, url: "http://example.com/test")

    assert_receive {:span, span(name: _, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    refute Map.has_key?(mapped_attributes, HTTPAttributes.http_request_resend_count())
  end

  test "non-standard HTTP method does not crash and sets _OTHER with method_original" do
    client =
      Tesla.client(
        [{Tesla.Middleware.OpenTelemetry, []}],
        fn env -> {:ok, %{env | status: 200}} end
      )

    Tesla.request(client, method: :purge, url: "http://example.com/cache")

    assert_receive {:span, span(name: "HTTP", kind: :client, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)
    assert mapped_attributes[HTTPAttributes.http_request_method()] == :_OTHER
    assert mapped_attributes[HTTPAttributes.http_request_method_original()] == "PURGE"
    assert mapped_attributes[HTTPAttributes.http_response_status_code()] == 200
  end

  test "raises on invalid options" do
    assert_raise NimbleOptions.ValidationError, fn ->
      client =
        Tesla.client(
          [{Tesla.Middleware.OpenTelemetry, invalid_option: true}],
          fn env -> {:ok, env} end
        )

      Tesla.get(client, "/")
    end
  end

  test "raises on invalid opt_in_attrs value" do
    assert_raise NimbleOptions.ValidationError, fn ->
      client =
        Tesla.client(
          [{Tesla.Middleware.OpenTelemetry, opt_in_attrs: [:not_a_valid_attr]}],
          fn env -> {:ok, env} end
        )

      Tesla.get(client, "/")
    end
  end

  defp client(opts \\ []) do
    [{Tesla.Middleware.OpenTelemetry, opts}]
    |> Tesla.client(fn env -> {:ok, env} end)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
