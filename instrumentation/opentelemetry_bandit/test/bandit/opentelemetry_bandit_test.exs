defmodule OpentelemetryBanditTest do
  use ExUnit.Case, async: true

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  use ServerHelper

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  describe "http integration" do
    test "default span generation for 200" do
      Req.get("http://localhost:4000/hello")

      assert_receive {:span,
                      span(
                        name: "HTTP GET /hello",
                        kind: :server,
                        status: {:status, :ok, _},
                        attributes: attributes
                      )}

      assert %{
               "net.peer.name": "localhost",
               "http.method": "GET",
               "http.target": "/hello",
               "http.scheme": :http,
               "http.status_code": 200
             } = :otel_attributes.map(attributes)
    end

    test "default span generation for 200 without user-agent" do
      {:ok, {{_, 200, _}, _, _}} =
        :httpc.request(:get, {~c"http://localhost:4000/hello", []}, [], [])

      assert_receive {:span,
                      span(
                        name: "HTTP GET /hello",
                        kind: :server,
                        status: {:status, :ok, _},
                        attributes: attributes
                      )}

      assert %{
               "net.peer.name": "localhost",
               "http.method": "GET",
               "http.target": "/hello",
               "http.scheme": :http,
               "http.status_code": 200,
               "http.client_ip": "127.0.0.1"
             } = :otel_attributes.map(attributes)
    end

    test "default span generation for 200 with x-forwarded-for" do
      Req.get("http://localhost:4000/hello", headers: %{x_forwarded_for: "127.0.0.1"})

      assert_receive {:span,
                      span(
                        name: "HTTP GET /hello",
                        kind: :server,
                        status: {:status, :ok, _},
                        attributes: attributes
                      )}

      assert %{
               "net.peer.name": "localhost",
               "http.method": "GET",
               "http.target": "/hello",
               "http.scheme": :http,
               "http.status_code": 200,
               "http.client_ip": "127.0.0.1"
             } = :otel_attributes.map(attributes)
    end

    test "default span generation for halted connection" do
      Req.get("http://localhost:4000/fail", retry: false)

      assert_receive {:span,
                      span(
                        name: "HTTP GET /fail",
                        kind: :server,
                        status: {:status, :ok, _},
                        attributes: attributes
                      )}

      assert %{
               "net.peer.name": "localhost",
               "http.method": "GET",
               "http.target": "/fail",
               "http.scheme": :http,
               "http.status_code": 500
             } = :otel_attributes.map(attributes)
    end

    test "default span generation for 500 response" do
      :telemetry.execute(
        [:bandit, :request, :stop],
        %{duration: 444, resp_body_bytes: 10},
        %{
          conn: nil,
          status: 500,
          error: "Internal Server Error",
          method: "GET",
          request_target: {nil, nil, nil, "/not_existing_route"}
        }
      )

      assert_receive {:span,
                      span(
                        name: "HTTP GET /not_existing_route",
                        kind: :error,
                        status: {:status, :error, "Internal Server Error"},
                        attributes: attributes
                      )}

      assert %{
               "http.url": _,
               "http.method": "GET",
               "http.status_code": 500,
               "http.response_content_length": 10,
               "net.transport": :"IP.TCP"
             } = :otel_attributes.map(attributes)
    end

    test "span when request_target is empty" do
      :telemetry.execute(
        [:bandit, :request, :stop],
        %{duration: 444, resp_body_bytes: 10},
        %{
          conn: nil,
          status: 500,
          error: "Internal Server Error",
          method: "GET",
          request_target: nil
        }
      )

      assert_receive {:span,
                      span(
                        name: "HTTP GET",
                        kind: :error,
                        status: {:status, :error, "Internal Server Error"},
                        attributes: attributes
                      )}

      assert %{
               "http.url": _,
               "http.method": "GET",
               "http.status_code": 500,
               "http.response_content_length": 10,
               "net.transport": :"IP.TCP"
             } = :otel_attributes.map(attributes)
    end

    test "exception catch span" do
      Req.get("http://localhost:4000/exception", retry: false)

      assert_receive {:span,
                      span(
                        name: "HTTP exception RuntimeError",
                        kind: :error,
                        status: {:status, :error, _}
                      )}
    end
  end

  describe "websocket integration" do
    test "span when request finished successfully" do
      :telemetry.execute(
        [:bandit, :websocket, :stop],
        %{
          duration: 444,
          send_binary_frame_bytes: 10,
          recv_binary_frame_bytes: 15
        },
        %{}
      )

      assert_receive {:span,
                      span(
                        name: "Websocket",
                        kind: :server,
                        status: {:status, :ok, _},
                        attributes: attributes
                      )}

      assert %{
               "net.transport": :websocket,
               "websocket.recv.binary.frame.bytes": 10,
               "websocket.send.binary.frame.bytes": 15
             } = :otel_attributes.map(attributes)
    end

    test "span when error is set" do
      :telemetry.execute(
        [:bandit, :websocket, :stop],
        %{
          duration: 444,
          send_binary_frame_bytes: 10,
          recv_binary_frame_bytes: 15
        },
        %{error: "Internal Server Error"}
      )

      assert_receive {:span,
                      span(
                        name: "Websocket",
                        kind: :error,
                        status: {:status, :error, _},
                        attributes: attributes
                      )}

      assert %{
               "net.transport": :websocket,
               "websocket.recv.binary.frame.bytes": 10,
               "websocket.send.binary.frame.bytes": 15
             } = :otel_attributes.map(attributes)
    end
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    {:ok, _} = start_supervised({Bandit, plug: __MODULE__, port: 4000, startup_log: false})

    OpentelemetryBandit.setup()

    :ok
  end

  def hello(conn) do
    conn |> send_resp(200, "OK")
  end

  def fail(conn) do
    conn |> send_resp(500, "Internal Server Error") |> halt()
  end

  def exception(_conn) do
    raise "boom"
  end
end
