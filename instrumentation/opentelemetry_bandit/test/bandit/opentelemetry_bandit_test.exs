defmodule OpentelemetryBanditTest do
  use ExUnit.Case, async: false

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ClientAttributes
  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.ExceptionAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpenTelemetry.SemConv.URLAttributes
  alias OpenTelemetry.SemConv.UserAgentAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes

  import ExUnit.CaptureLog, only: [capture_log: 1]

  use ServerHelper

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    on_exit(fn -> :telemetry.detach({OpentelemetryBandit, :otel_bandit}) end)
    :ok
  end

  def start_server(port \\ Enum.random(4000..10_000)) do
    {:ok, _} = start_supervised({Bandit, plug: __MODULE__, port: port})
    port
  end

  test "validates opt-ins" do
    err =
      catch_error(
        OpentelemetryBandit.setup(
          opt_in_attrs: [
            ClientAttributes.client_port(),
            :unsupported,
            HTTPAttributes.http_request_body_size()
          ]
        )
      )

    assert is_struct(err, NimbleOptions.ValidationError)

    assert String.starts_with?(
             err.message,
             "invalid list in :opt_in_attrs option: invalid value for list element at position"
           )
  end

  describe "GET" do
    test "basic request with default options" do
      OpentelemetryBandit.setup()
      port = start_server()

      Req.get("http://localhost:#{port}/hello",
        params: [a: 1, b: "abc"],
        headers: %{
          "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
          "tracestate" => "congo=t61rcWkgMzE"
        }
      )

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        attributes: span_attrs,
                        parent_span_id: 13_235_353_014_750_950_193
                      )}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ClientAttributes.client_address(), "127.0.0.1"},
        {HTTPAttributes.http_request_method(), :GET},
        {HTTPAttributes.http_response_status_code(), 200},
        {NetworkAttributes.network_peer_address(), "127.0.0.1"},
        {URLAttributes.url_path(), "/hello"},
        {URLAttributes.url_query(), "a=1&b=abc"},
        {URLAttributes.url_scheme(), :http}
      ]

      for {attr, val} <- expected_attrs do
        assert Map.get(attrs, attr) == val
      end

      user_agent = Map.get(attrs, UserAgentAttributes.user_agent_original())
      assert String.starts_with?(user_agent, "req/")
    end

    test "public endpoint true" do
      OpentelemetryBandit.setup(public_endpoint: true)
      port = start_server()

      Req.get("http://localhost:#{port}/hello",
        headers: %{
          "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
          "tracestate" => "congo=t61rcWkgMzE"
        }
      )

      refute_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        parent_span_id: 13_235_353_014_750_950_193
                      )}

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        links: links,
                        parent_span_id: :undefined
                      )}

      assert length(:otel_links.list(links)) == 1
    end

    def public_endpoint_fn(_conn, _opts) do
      System.get_env("TENANT") != "internal"
    end

    test "public endpoint fn" do
      OpentelemetryBandit.setup(public_endpoint_fn: {__MODULE__, :public_endpoint_fn, []})
      port = start_server()

      System.put_env("TENANT", "customer")

      Req.get("http://localhost:#{port}/hello",
        headers: %{
          "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
          "tracestate" => "congo=t61rcWkgMzE"
        }
      )

      refute_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        parent_span_id: 13_235_353_014_750_950_193
                      )}

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        links: links,
                        parent_span_id: :undefined
                      )}

      assert length(:otel_links.list(links)) == 1

      System.put_env("TENANT", "internal")

      Req.get("http://localhost:#{port}/hello",
        headers: %{
          "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
          "tracestate" => "congo=t61rcWkgMzE"
        }
      )

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        parent_span_id: 13_235_353_014_750_950_193
                      )}

      refute_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        parent_span_id: :undefined
                      )}

      System.delete_env("PUBLIC")
    end

    test "with all opt-ins" do
      OpentelemetryBandit.setup(
        opt_in_attrs: [
          ClientAttributes.client_port(),
          HTTPAttributes.http_request_body_size(),
          HTTPAttributes.http_response_body_size(),
          NetworkAttributes.network_local_address(),
          NetworkAttributes.network_local_port(),
          NetworkAttributes.network_transport()
        ],
        request_headers: ["test-header"],
        response_headers: ["content-type"]
      )

      port = start_server()

      Req.get!("http://localhost:#{port}/with_body",
        headers: %{"test-header" => "request header"}
      )

      assert_receive {:span,
                      span(
                        name: :GET,
                        attributes: span_attrs
                      )}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ClientAttributes.client_address(), "127.0.0.1"},
        {HTTPAttributes.http_request_body_size(), 0},
        {HTTPAttributes.http_request_method(), :GET},
        {HTTPAttributes.http_response_body_size(), 29},
        {HTTPAttributes.http_response_status_code(), 200},
        {String.to_atom("#{HTTPAttributes.http_request_header()}.test-header"),
         ["request header"]},
        {String.to_atom("#{HTTPAttributes.http_response_header()}.content-type"),
         ["application/json; charset=utf-8"]},
        {NetworkAttributes.network_local_address(), "localhost"},
        {NetworkAttributes.network_local_port(), port},
        {NetworkAttributes.network_peer_address(), "127.0.0.1"},
        {URLAttributes.url_path(), "/with_body"},
        {URLAttributes.url_scheme(), :http}
      ]

      for {attr, expected} <- expected_attrs do
        actual = Map.get(attrs, attr)
        assert expected == actual, "#{attr} expected #{expected} got #{actual}"
      end

      user_agent = Map.get(attrs, UserAgentAttributes.user_agent_original())
      assert String.starts_with?(user_agent, "req/")

      client_port = Map.get(attrs, ClientAttributes.client_port())
      assert is_integer(client_port)
    end

    def custom_client_header_sort(h1, h2) do
      h1_priority = custom_client_header_priority(h1)
      h2_priority = custom_client_header_priority(h2)

      case {h1_priority, h2_priority} do
        {h1, h2} when h1 <= h2 ->
          true

        {h1, h2} when h1 > h2 ->
          false
      end
    end

    defp custom_client_header_priority({header_name, _value}) do
      case header_name do
        "custom-client" -> 1
        "x-forwarded-for" -> 2
      end
    end

    def custom_server_header_sort(h1, h2) do
      h1_priority = custom_server_header_priority(h1)
      h2_priority = custom_server_header_priority(h2)

      case {h1_priority, h2_priority} do
        {h1, h2} when h1 <= h2 ->
          true

        {h1, h2} when h1 > h2 ->
          false
      end
    end

    defp custom_server_header_priority({header_name, _value}) do
      case header_name do
        "custom-host" -> 1
        "x-forwarded-host" -> 2
        "forwarded" -> 3
        _ -> 4
      end
    end

    def custom_scheme_header_sort(h1, h2) do
      h1_priority = custom_scheme_header_priority(h1)
      h2_priority = custom_scheme_header_priority(h2)

      case {h1_priority, h2_priority} do
        {h1, h2} when h1 <= h2 ->
          true

        {h1, h2} when h1 > h2 ->
          false
      end
    end

    defp custom_scheme_header_priority({header_name, _value}) do
      case header_name do
        "custom-scheme" -> 1
        "x-forwarded-proto" -> 2
      end
    end

    test "with custom header settings" do
      opts = [
        client_address_headers: ["x-forwarded-for", "custom-client"],
        client_headers_sort_fn: &__MODULE__.custom_client_header_sort/2,
        scheme_headers: ["custom-scheme", "x-forwarded-proto"],
        scheme_headers_sort_fn: &__MODULE__.custom_scheme_header_sort/2,
        server_address_headers: ["custom-host", "forwarded", "host"],
        server_headers_sort_fn: &__MODULE__.custom_server_header_sort/2
      ]

      OpentelemetryBandit.setup(opts)
      port = start_server()

      Req.get("http://localhost:#{port}/hello",
        headers: %{
          "forwarded" =>
            ~S(host=developer.mozilla.org:4321; for=192.0.2.60, for="[2001:db8:cafe::17]";proto=http;by=203.0.113.43),
          "x-forwarded-proto" => "http",
          "custom-scheme" => "https",
          "custom-client" => "23.23.23.23",
          "traceparent" => "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
          "tracestate" => "congo=t61rcWkgMzE"
        }
      )

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        attributes: span_attrs
                      )}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ClientAttributes.client_address(), "23.23.23.23"},
        {HTTPAttributes.http_request_method(), :GET},
        {HTTPAttributes.http_response_status_code(), 200},
        {NetworkAttributes.network_peer_address(), "127.0.0.1"},
        {ServerAttributes.server_address(), "developer.mozilla.org"},
        {ServerAttributes.server_port(), 4321},
        {URLAttributes.url_scheme(), :https}
      ]

      for {attr, val} <- expected_attrs do
        assert Map.get(attrs, attr) == val
      end

      user_agent = Map.get(attrs, UserAgentAttributes.user_agent_original())
      assert String.starts_with?(user_agent, "req/")
    end

    test "with missing user-agent" do
      OpentelemetryBandit.setup()
      port = start_server()

      {:ok, {{_, 200, _}, _, _}} =
        :httpc.request(:get, {~c"http://localhost:#{port}/hello", []}, [], [])

      assert_receive {:span, span(attributes: span_attrs)}

      attrs = :otel_attributes.map(span_attrs)

      assert Map.get(attrs, UserAgentAttributes.user_agent_original()) == ""
    end

    test "with exception" do
      OpentelemetryBandit.setup()
      port = start_server()

      capture_log(fn ->
        Req.get("http://localhost:#{port}/arithmetic_error", retry: false)
      end)

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span,
                      span(
                        name: :GET,
                        attributes: span_attrs,
                        events: events,
                        status: ^expected_status
                      )}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ClientAttributes.client_address(), "127.0.0.1"},
        {ErrorAttributes.error_type(), ArithmeticError},
        {HTTPAttributes.http_request_method(), :GET},
        {HTTPAttributes.http_response_status_code(), 500},
        {NetworkAttributes.network_peer_address(), "127.0.0.1"},
        {URLAttributes.url_path(), "/arithmetic_error"},
        {URLAttributes.url_scheme(), :http}
      ]

      for {attr, val} <- expected_attrs do
        assert Map.get(attrs, attr) == val
      end

      [
        event(
          name: :exception,
          attributes: event_attributes
        )
      ] = :otel_events.list(events)

      assert [
               ExceptionAttributes.exception_message(),
               ExceptionAttributes.exception_stacktrace(),
               ExceptionAttributes.exception_type()
             ] ==
               Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
    end

    test "with throw" do
      OpentelemetryBandit.setup()
      port = start_server()

      capture_log(fn ->
        Req.get("http://localhost:#{port}/throw_error", retry: false)
      end)

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span,
                      span(
                        name: :GET,
                        attributes: span_attributes,
                        events: events,
                        status: ^expected_status
                      )}

      for {attribute, expected_value} <- [
            {ClientAttributes.client_address(), "127.0.0.1"},
            {ErrorAttributes.error_type(), "something"},
            {HTTPAttributes.http_request_method(), :GET},
            {HTTPAttributes.http_response_status_code(), 500},
            {NetworkAttributes.network_peer_address(), "127.0.0.1"},
            {URLAttributes.url_path(), "/throw_error"},
            {URLAttributes.url_scheme(), :http}
          ] do
        assert Map.get(:otel_attributes.map(span_attributes), attribute) == expected_value
      end

      assert [] = :otel_events.list(events)
    end

    test "with exit" do
      OpentelemetryBandit.setup()
      port = start_server()

      capture_log(fn ->
        Req.get("http://localhost:#{port}/exit_error", retry: false)
      end)

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span,
                      span(
                        name: :GET,
                        attributes: span_attributes,
                        events: events,
                        status: ^expected_status
                      )}

      for {attribute, expected_value} <- [
            {ClientAttributes.client_address(), "127.0.0.1"},
            {ErrorAttributes.error_type(), :abnormal_reason},
            {HTTPAttributes.http_request_method(), :GET},
            {HTTPAttributes.http_response_status_code(), 500},
            {NetworkAttributes.network_peer_address(), "127.0.0.1"},
            {URLAttributes.url_path(), "/exit_error"},
            {URLAttributes.url_scheme(), :http}
          ] do
        assert Map.get(:otel_attributes.map(span_attributes), attribute) == expected_value
      end

      assert [] = :otel_events.list(events)
    end

    test "with halted request" do
      OpentelemetryBandit.setup()
      port = start_server()

      Req.get("http://localhost:#{port}/halted", retry: false)

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span,
                      span(
                        name: :GET,
                        kind: :server,
                        attributes: span_attrs,
                        status: ^expected_status
                      )}

      attrs = :otel_attributes.map(span_attrs)

      expected_attrs = [
        {ErrorAttributes.error_type(), "500"},
        {HTTPAttributes.http_response_status_code(), 500},
        {URLAttributes.url_scheme(), :http}
      ]

      for {attr, val} <- expected_attrs do
        assert Map.get(attrs, attr) == val
      end
    end
  end

  def hello(conn) do
    conn |> send_resp(200, "OK")
  end

  def with_body(conn) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(%{"a" => "b"}))
  end

  def halted(conn) do
    conn |> send_resp(500, "Internal Server Error") |> halt()
  end

  def arithmetic_error(_conn) do
    1 / 0
  end

  def throw_error(_conn) do
    throw("something")
  end

  def exit_error(_conn) do
    exit(:abnormal_reason)
  end
end
