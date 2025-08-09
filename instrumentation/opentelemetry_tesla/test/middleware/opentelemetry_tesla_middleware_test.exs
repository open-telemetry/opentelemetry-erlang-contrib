defmodule Tesla.Middleware.OpenTelemetryTest do
  use ExUnit.Case
  require Record

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

      assert_receive {:span, span(name: "/users/:id", attributes: _attributes)}
    end

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

      assert_receive {:span, span(name: "/users/:id", attributes: _attributes)}
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

      assert_receive {:span, span(name: "HTTP GET", attributes: _attributes)}
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
          {Tesla.Middleware.OpenTelemetry, span_name: "POST :my-high-cardinality-url"},
          Tesla.Middleware.PathParams
        ])

      Tesla.get(client, "/users/:id", opts: [path_params: [id: "3"]])

      assert_receive {:span, span(name: "POST :my-high-cardinality-url", attributes: _attributes)}
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
          {Tesla.Middleware.OpenTelemetry,
           span_name: fn env ->
             "#{String.upcase(to_string(env.method))} potato"
           end},
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

    assert_receive {:span, span(name: "HTTP GET", attributes: _attributes)}
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

    assert_receive {:span, span(status: {:status, :error, ""})}
  end

  test "Marks Span status as :error if error status is within `mark_status_ok` opt list",
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

  test "Marks Span status as :ok unless error status is within `mark_status_ok` opt list",
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

  test "Appends query string parameters to http.url attribute", %{
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

    assert mapped_attributes[:"http.url"] ==
             "http://localhost:#{bypass.port}/users/2?token=some-token&array%5B%5D=foo&array%5B%5D=bar"
  end

  test "http.url attribute is correct when request doesn't contain query string parameters", %{
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

    assert mapped_attributes[:"http.url"] ==
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
    assert %{"http.target": "/users/2"} = :otel_attributes.map(attributes)
  end

  test "Records http.response_content_length param into the span", %{
    bypass: bypass,
    base_url: base_url
  } do
    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, "HELLO 👋")
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

    {response_size, _} = Integer.parse(mapped_attributes[:"http.response_content_length"])
    assert response_size == byte_size("HELLO 👋")
  end

  describe "trace propagation" do
    test "injects distributed tracing headers by default" do
      {:ok, env} = Tesla.get(client(), "/propagate-traces")

      assert traceparent = Tesla.get_header(env, "traceparent")
      assert is_binary(traceparent)

      assert_receive {:span, span(name: _name, attributes: attributes)}
      assert %{"http.target": "/propagate-traces"} = :otel_attributes.map(attributes)
    end

    test "optionally disable propagation but keep span report" do
      {:ok, env} = Tesla.get(client(propagator: :none), "/propagate-traces")

      refute Tesla.get_header(env, "traceparent")

      assert_receive {:span, span(name: _name, attributes: attributes)}
      assert %{"http.target": "/propagate-traces"} = :otel_attributes.map(attributes)
    end
  end

  defp client(opts \\ []) do
    [{Tesla.Middleware.OpenTelemetry, opts}]
    |> Tesla.client(fn env -> {:ok, env} end)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
