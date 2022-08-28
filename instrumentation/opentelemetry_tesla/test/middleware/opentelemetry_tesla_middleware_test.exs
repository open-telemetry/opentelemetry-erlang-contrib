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

    {:ok, bypass: bypass}
  end

  test "it records a generic span name if opentelemetry middleware is configured before path params middleware",
       %{
         bypass: bypass
       } do
    defmodule TestClient do
      def get(client) do
        params = [id: '3']

        Tesla.get(client, "/users/:id", opts: [path_params: params])
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users/3", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get()

    assert_receive {:span, span(name: "/users/:id", attributes: _attributes)}
  end

  test "Records spans for Tesla HTTP client", %{bypass: bypass} do
    defmodule TestClient do
      def get(client) do
        Tesla.get(client, "/users/")
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get()

    assert_receive {:span, span(name: "HTTP GET", attributes: _attributes)}
  end

  test "Marks Span status as :error when HTTP request fails", %{bypass: bypass} do
    defmodule TestClient do
      def get(client) do
        Tesla.get(client, "/users/")
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users", fn conn ->
      Plug.Conn.resp(conn, 500, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get()

    assert_receive {:span, span(status: {:status, :error, ""})}
  end

  test "Marks Span status as :errors when max redirects are exceeded", %{bypass: bypass} do
    defmodule TestClient do
      def get(client) do
        Tesla.get(client, "/users/")
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          {Tesla.Middleware.FollowRedirects, max_redirects: 1}
        ]

        Tesla.client(middleware)
      end
    end

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

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get()

    assert_receive {:span, span(status: {:status, :error, ""})}
  end

  test "Appends query string parameters to http.url attribute", %{bypass: bypass} do
    defmodule TestClient do
      def get(client, id) do
        params = [id: id]
        Tesla.get(client, "/users/:id", opts: [path_params: params])
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams,
          {Tesla.Middleware.Query, [token: "some-token", array: ["foo", "bar"]]}
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get("2")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[:"http.url"] ==
             "http://localhost:#{bypass.port}/users/2?token=some-token&array%5B%5D=foo&array%5B%5D=bar"
  end

  test "http.url attribute is correct when request doesn't contain query string parameters", %{
    bypass: bypass
  } do
    defmodule TestClient do
      def get(client, id) do
        params = [id: id]
        Tesla.get(client, "/users/:id", opts: [path_params: params])
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams,
          {Tesla.Middleware.Query, []}
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get("2")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    assert mapped_attributes[:"http.url"] ==
             "http://localhost:#{bypass.port}/users/2"
  end

  test "Handles url path arguments correctly", %{bypass: bypass} do
    defmodule TestClient do
      def get(client, id) do
        params = [id: id]
        Tesla.get(client, "/users/:id", opts: [path_params: params])
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams,
          {Tesla.Middleware.Query, [token: "some-token"]}
        ]

        Tesla.client(middleware)
      end
    end

    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 204, "")
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get("2")

    assert_receive {:span, span(name: _name, attributes: attributes)}
    assert %{"http.target": "/users/2"} = :otel_attributes.map(attributes)
  end

  test "Records http.response_content_length param into the span", %{bypass: bypass} do
    defmodule TestClient do
      def get(client, id) do
        params = [id: id]
        Tesla.get(client, "/users/:id", opts: [path_params: params])
      end

      def client(url) do
        middleware = [
          {Tesla.Middleware.BaseUrl, url},
          Tesla.Middleware.OpenTelemetry,
          Tesla.Middleware.PathParams,
          {Tesla.Middleware.Query, [token: "some-token"]}
        ]

        Tesla.client(middleware)
      end
    end

    response = "HELLO 👋"

    Bypass.expect_once(bypass, "GET", "/users/2", fn conn ->
      Plug.Conn.resp(conn, 200, response)
    end)

    bypass.port
    |> endpoint_url()
    |> TestClient.client()
    |> TestClient.get("2")

    assert_receive {:span, span(name: _name, attributes: attributes)}

    mapped_attributes = :otel_attributes.map(attributes)

    {response_size, _} = Integer.parse(mapped_attributes[:"http.response_content_length"])
    assert response_size == byte_size(response)
  end

  test "Injects distributed tracing headers" do
    OpentelemetryTelemetry.start_telemetry_span(
      "tracer_id",
      "my_label",
      %{},
      %{kind: :client}
    )

    assert {:ok,
            %Tesla.Env{
              headers: [
                {"traceparent", traceparent}
              ]
            }} =
             Tesla.Middleware.OpenTelemetry.call(
               %Tesla.Env{url: ""},
               [],
               "http://example.com"
             )

    assert is_binary(traceparent)
  end

  defp endpoint_url(port), do: "http://localhost:#{port}/"
end
