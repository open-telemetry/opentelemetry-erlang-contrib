defmodule OpentelemetryGrpc.ServerTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryGrpc.Server

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpentelemetryGrpc.TestSupport
  alias Testserver.V1.{TestService, HelloRequest}
  alias OpenTelemetry.SemConv.Incubating.RPCAttributes

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  @test_port 50054

  @grpc_status_ok RPCAttributes.rpc_grpc_status_code_values().ok
  @grpc_status_not_found RPCAttributes.rpc_grpc_status_code_values().not_found
  @grpc_status_internal RPCAttributes.rpc_grpc_status_code_values().internal
  @grpc_status_invalid_argument RPCAttributes.rpc_grpc_status_code_values().invalid_argument

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

    OpentelemetryGrpc.Server.setup()

    on_exit(fn ->
      for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)
    end)

    :ok
  end

  describe "setup/1" do
    setup do
      for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)

      on_exit(fn ->
        for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)
      end)

      :ok
    end

    test "sets up with default options" do
      :ok = OpentelemetryGrpc.Server.setup()

      assert handlers = :telemetry.list_handlers([])

      server_events = [
        [:grpc, :server, :rpc, :start],
        [:grpc, :server, :rpc, :stop],
        [:grpc, :server, :rpc, :exception]
      ]

      for event <- server_events do
        assert Enum.any?(handlers, &match?(%{event_name: ^event}, &1))
      end
    end

    test "sets up with child span relationship" do
      :ok = OpentelemetryGrpc.Server.setup(span_relationship: :child)

      assert handlers = :telemetry.list_handlers([])

      assert Enum.any?(
               handlers,
               &match?(
                 %{
                   event_name: [:grpc, :server, :rpc, :start],
                   config: %{span_relationship: :child}
                 },
                 &1
               )
             )
    end

    test "sets up with link span relationship" do
      :ok = OpentelemetryGrpc.Server.setup(span_relationship: :link)

      assert handlers = :telemetry.list_handlers([])

      assert Enum.any?(
               handlers,
               &match?(
                 %{
                   event_name: [:grpc, :server, :rpc, :start],
                   config: %{span_relationship: :link}
                 },
                 &1
               )
             )
    end

    test "sets up with none span relationship" do
      :ok = OpentelemetryGrpc.Server.setup(span_relationship: :none)

      assert handlers = :telemetry.list_handlers([])

      assert Enum.any?(
               handlers,
               &match?(
                 %{
                   event_name: [:grpc, :server, :rpc, :start],
                   config: %{span_relationship: :none}
                 },
                 &1
               )
             )
    end

    test "raises on invalid span_relationship option" do
      assert_raise NimbleOptions.ValidationError, fn ->
        OpentelemetryGrpc.Server.setup(span_relationship: :invalid)
      end
    end
  end

  describe "integration tests" do
    test "records span on gRPC server requests", %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      request = %HelloRequest{name: "ServerTest"}

      assert {:ok, _response} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :server,
                        attributes: attributes,
                        status: status
                      )}

      assert status == OpenTelemetry.status(:ok)

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"rpc.grpc.status_code" => @grpc_status_ok,
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      TestSupport.disconnect_client(channel)
    end
  end

  describe "error handling" do
    setup %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

      on_exit(fn -> TestSupport.disconnect_client(channel) end)

      {:ok, channel: channel}
    end

    test "records span with GRPC.RPCError (not_found)", %{channel: channel} do
      request = %HelloRequest{name: "TriggerNotFound"}

      assert {:error, _error} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :server,
                        status: status,
                        attributes: attributes
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"rpc.grpc.status_code" => @grpc_status_not_found,
               :"error.type" => "Elixir.GRPC.RPCError",
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      assert status == OpenTelemetry.status(:error, "User not found")
    end

    test "records span with GRPC.RPCError (invalid_argument)", %{channel: channel} do
      request = %HelloRequest{name: "TriggerInvalidArgument"}

      assert {:error, _error} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :server,
                        status: status,
                        attributes: attributes
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"rpc.grpc.status_code" => @grpc_status_invalid_argument,
               :"error.type" => "Elixir.GRPC.RPCError",
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      assert status == OpenTelemetry.status(:error, "Invalid name format")
    end

    test "records span with exception", %{channel: channel} do
      request = %HelloRequest{name: "TriggerException"}

      assert {:error, _error} = TestService.Stub.say_hello(channel, request)

      assert_receive {:span,
                      span(
                        name: "testserver.v1.TestService/SayHello",
                        kind: :server,
                        status: status,
                        attributes: attributes
                      )}

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"rpc.grpc.status_code" => @grpc_status_internal,
               :"error.type" => "Elixir.RuntimeError",
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      assert status == OpenTelemetry.status(:error, "Server crashed")
    end
  end
end
