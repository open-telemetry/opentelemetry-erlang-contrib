defmodule OpentelemetryReqTest do
  use ExUnit.Case, async: true
  doctest OpentelemetryReq

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias OpenTelemetry.SemConv.Incubating.URLAttributes

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecordp(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  defp client(opts \\ []) do
    Req.new()
    |> OpentelemetryReq.attach(opts)
  end

  test "basic request" do
    plug = fn conn ->
      conn
      |> Plug.Conn.put_status(200)
      |> Req.Test.json(%{id: 3})
    end

    Req.get!(client(),
      plug: plug,
      url: "http://localtest:8080/users/:id",
      path_params: [id: 3],
      params: [a: "b"]
    )

    assert_receive {:span,
                    span(
                      name: :GET,
                      kind: :client,
                      attributes: span_attrs
                    )}

    attrs = :otel_attributes.map(span_attrs)

    expected_attrs = [
      {HTTPAttributes.http_request_method(), :GET},
      {HTTPAttributes.http_response_status_code(), 200},
      {ServerAttributes.server_address(), "localtest"},
      {ServerAttributes.server_port(), 8080},
      {URLAttributes.url_full(), "http://localtest:8080/users/3?a=b"}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{actual}"
    end
  end

  # with all opt-ins
  test "with all other opt-ins and header options", %{bypass: bypass} do
    Bypass.expect_once(bypass, "POST", "/users/3", fn conn ->
      conn
      |> Plug.Conn.put_status(200)
      |> Req.Test.json(%{user_id: 3})
    end)

    fields = [a: 1, b: {"2", filename: "b.txt"}]

    client =
      client(%{
        opt_in_attrs: [
          HTTPAttributes.http_request_body_size(),
          HTTPAttributes.http_response_body_size(),
          NetworkAttributes.network_transport(),
          URLAttributes.url_scheme(),
          URLAttributes.url_template(),
          UserAgentAttributes.user_agent_original()
        ]
      })

    Req.post!(client,
      url: "http://localhost:#{bypass.port}/users/:user_id",
      path_params: [user_id: 3],
      params: [a: "b"],
      headers: %{test_header: "request header"},
      form_multipart: fields,
      request_header_attrs: ["test-header", "user-agent"],
      response_header_attrs: ["content-type"]
    )

    assert_receive {:span,
                    span(
                      name: "POST /users/:user_id",
                      kind: :client,
                      attributes: span_attrs
                    )}

    attrs = :otel_attributes.map(span_attrs)

    expected_attrs = [
      {HTTPAttributes.http_request_method(), :POST},
      {HTTPAttributes.http_response_status_code(), 200},
      {HTTPAttributes.http_request_body_size(), 224},
      {HTTPAttributes.http_response_body_size(), 13},
      {NetworkAttributes.network_transport(), :tcp},
      {String.to_atom("#{HTTPAttributes.http_request_header()}.test-header"), ["request header"]},
      {String.to_atom("#{HTTPAttributes.http_response_header()}.content-type"),
       ["application/json; charset=utf-8"]},
      {ServerAttributes.server_address(), "localhost"},
      {ServerAttributes.server_port(), bypass.port},
      {URLAttributes.url_full(), "http://localhost:#{bypass.port}/users/3?a=b"},
      {URLAttributes.url_scheme(), :http},
      {URLAttributes.url_template(), "/users/:user_id"}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{inspect(actual)}"
    end

    user_agent = Map.get(attrs, UserAgentAttributes.user_agent_original())
    assert String.starts_with?(user_agent, "req/")
  end

  describe "errors" do
    test "timeout exception error" do
      Req.get(client(),
        plug: fn conn ->
          Req.Test.transport_error(conn, :timeout)
        end,
        retry: false,
        url: "/"
      )

      expected_status = OpenTelemetry.status(:error, "timeout")

      assert_receive {:span, span(attributes: span_attrs, status: ^expected_status)}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ErrorAttributes.error_type(), Req.TransportError}
      ]

      for {attr, expected} <- expected_attrs do
        actual = Map.get(attrs, attr)
        assert expected == actual, "#{attr} expected #{expected} got #{inspect(actual)}"
      end
    end

    test "4xx level error", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/", fn conn ->
        conn
        |> Plug.Conn.put_status(404)
        |> Req.Test.text("not found")
      end)

      Req.get(client(),
        retry: false,
        url: "http://localhost:#{bypass.port}"
      )

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span, span(attributes: span_attrs, status: ^expected_status)}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ErrorAttributes.error_type(), "404"}
      ]

      for {attr, expected} <- expected_attrs do
        actual = Map.get(attrs, attr)
        assert expected == actual, "#{attr} expected #{expected} got #{inspect(actual)}"
      end
    end

    test "5xx level error", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/", fn conn ->
        conn
        |> Plug.Conn.put_status(500)
        |> Req.Test.text("internal server error")
      end)

      Req.get(client(),
        retry: false,
        url: "http://localhost:#{bypass.port}"
      )

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span, span(attributes: span_attrs, status: ^expected_status)}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ErrorAttributes.error_type(), "500"}
      ]

      for {attr, expected} <- expected_attrs do
        actual = Map.get(attrs, attr)
        assert expected == actual, "#{attr} expected #{expected} got #{inspect(actual)}"
      end
    end
  end

  # exception test
  def ok_resp(conn) do
    conn |> Req.Test.text("ok")
  end

  describe "span name" do
    test "with no path params" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "/"
      )

      assert_receive {:span, span(name: :GET)}
    end

    test "with path params but template attr not set" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "http://localtest:8080/users/:id",
        path_params: [id: 3],
        params: [a: "b"]
      )

      assert_receive {:span, span(name: :GET)}
    end

    test "with path params" do
      Req.get!(client(opt_in_attrs: [URLAttributes.url_template()]),
        plug: &ok_resp/1,
        url: "http://localtest:8080/users/:id",
        path_params: [id: 3],
        params: [a: "b"]
      )

      assert_receive {:span, span(name: "GET /users/:id")}
    end

    test "with span name attach option" do
      Req.get!(client(span_name: "test"),
        plug: &ok_resp/1,
        url: "http://localtest:8080/users/:id",
        path_params: [id: 3],
        params: [a: "b"]
      )

      assert_receive {:span, span(name: "test")}
    end

    test "with span name request option" do
      Req.get!(client(span_name: "test"),
        plug: &ok_resp/1,
        url: "http://localtest:8080/users/:id",
        path_params: [id: 3],
        params: [a: "b"],
        span_name: "overridden"
      )

      assert_receive {:span, span(name: "overridden")}
    end
  end

  describe "ports" do
    test "when port present" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "http://localtest:8080"
      )

      assert_receive {:span, span(attributes: span_attrs)}

      attrs = :otel_attributes.map(span_attrs)
      assert 8080 == Map.get(attrs, ServerAttributes.server_port())
    end

    test "when port not set and no scheme" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "/ok"
      )

      assert_receive {:span, span(attributes: span_attrs)}

      attrs = :otel_attributes.map(span_attrs)
      assert 80 == Map.get(attrs, ServerAttributes.server_port())
    end

    test "when port not set and http scheme" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "http://localtest"
      )

      assert_receive {:span, span(attributes: span_attrs)}

      attrs = :otel_attributes.map(span_attrs)
      assert 80 == Map.get(attrs, ServerAttributes.server_port())
    end

    test "when port not set and https scheme" do
      Req.get!(client(),
        plug: &ok_resp/1,
        url: "https://localtest"
      )

      assert_receive {:span, span(attributes: span_attrs)}

      attrs = :otel_attributes.map(span_attrs)
      assert 443 == Map.get(attrs, ServerAttributes.server_port())
    end
  end

  describe "propagation" do
    test "off by default" do
      plug = fn conn ->
        assert [] == Plug.Conn.get_req_header(conn, "traceparent")

        ok_resp(conn)
      end

      Req.get!(client(),
        plug: plug,
        url: "/"
      )
    end

    test "enabled in attach" do
      plug = fn conn ->
        assert 1 == length(Plug.Conn.get_req_header(conn, "traceparent"))

        ok_resp(conn)
      end

      Req.get!(client(propagate_trace_headers: true),
        plug: plug,
        url: "/"
      )
    end

    test "enabled in request" do
      plug = fn conn ->
        assert 1 == length(Plug.Conn.get_req_header(conn, "traceparent"))

        ok_resp(conn)
      end

      Req.get!(client(),
        plug: plug,
        url: "/",
        propagate_trace_headers: true
      )
    end

    test "disabled in request" do
      plug = fn conn ->
        assert 0 == length(Plug.Conn.get_req_header(conn, "traceparent"))

        ok_resp(conn)
      end

      Req.get!(client(propagate_trace_headers: true),
        plug: plug,
        url: "/",
        propagate_trace_headers: false
      )
    end
  end
end
