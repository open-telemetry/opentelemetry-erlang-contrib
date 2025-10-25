defmodule OpentelemetryGrpc.ClientTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryGrpc.Client

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpentelemetryGrpc.TestSupport
  alias Testserver.V1.{TestService, HelloRequest}

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  @test_port 50053

  setup_all do
    Application.ensure_all_started(:telemetry)
    Application.ensure_all_started(:grpc)

    port = TestSupport.start_server(@test_port)

    on_exit(fn ->
      TestSupport.stop_server(port)
    end)

    {:ok, port: port}
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    OpentelemetryGrpc.Client.setup()

    on_exit(fn ->
      for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)
    end)

    :ok
  end

  describe "setup/0" do
    test "attaches telemetry handlers for client events" do
      assert handlers = :telemetry.list_handlers([])

      client_events = [
        [:grpc, :client, :rpc, :start],
        [:grpc, :client, :rpc, :stop],
        [:grpc, :client, :rpc, :exception]
      ]

      for event <- client_events do
        assert Enum.any?(handlers, &match?(%{event_name: ^event}, &1))
      end
    end
  end

  describe "integration tests" do
    test "records span on successful gRPC client requests", %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      request = %HelloRequest{name: "ClientTest"}

      {:ok, response} = TestService.Stub.say_hello(channel, request)
      assert response.message == "Hello, ClientTest!"

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :client,
                        attributes: attributes,
                        status: status
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"server.address" => "127.0.0.1",
               :"server.port" => port,
               :"rpc.grpc.status_code" => 0
             }

      assert status == OpenTelemetry.status(:ok)

      TestSupport.disconnect_client(channel)
    end

    test "records span on successful gRPC client requests with return_headers: true", %{
      port: port
    } do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      request = %HelloRequest{name: "ClientTest"}

      # Test that the implementation correctly handles the 3-tuple format returned
      # when return_headers: true option is used. The gRPC stub returns
      # {:ok, response, metadata} instead of {:ok, response}, and we need to
      # ensure the status code is still properly recorded.
      {:ok, response, metadata} =
        TestService.Stub.say_hello(channel, request, return_headers: true)

      assert response.message == "Hello, ClientTest!"
      assert is_map(metadata.headers)
      assert is_map(metadata.trailers)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :client,
                        attributes: attributes,
                        status: status
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"server.address" => "127.0.0.1",
               :"server.port" => port,
               :"rpc.grpc.status_code" => 0
             }

      assert status == OpenTelemetry.status(:ok)

      TestSupport.disconnect_client(channel)
    end
  end

  describe "error handling" do
    test "records span with GRPC.RPCError (not_found)", %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      request = %HelloRequest{name: "TriggerNotFound"}

      assert {:error, _error} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :client,
                        attributes: attributes,
                        status: status
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"server.address" => "127.0.0.1",
               :"server.port" => port,
               :"rpc.grpc.status_code" => 5,
               :"error.type" => "grpc_error"
             }

      assert status == OpenTelemetry.status(:error, "User not found")

      TestSupport.disconnect_client(channel)
    end

    test "records span with GRPC.RPCError (invalid_argument)", %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      request = %HelloRequest{name: "TriggerInvalidArgument"}

      assert {:error, _error} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :client,
                        attributes: attributes,
                        status: status
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"server.address" => "127.0.0.1",
               :"server.port" => port,
               :"rpc.grpc.status_code" => 3,
               :"error.type" => "grpc_error"
             }

      assert status == OpenTelemetry.status(:error, "Invalid name format")

      TestSupport.disconnect_client(channel)
    end
  end
end
