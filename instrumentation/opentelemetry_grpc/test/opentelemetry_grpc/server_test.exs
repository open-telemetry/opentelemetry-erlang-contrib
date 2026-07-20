defmodule OpentelemetryGrpc.ServerTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryGrpc.Server

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

  @test_port 50054

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

      assert status == :undefined

      assert :otel_attributes.map(attributes) == %{
               :"rpc.system" => :grpc,
               :"rpc.service" => "testserver.v1.TestService",
               :"rpc.method" => "SayHello",
               :"network.protocol.name" => "http",
               :"network.protocol.version" => "2",
               :"network.transport" => "tcp",
               :"rpc.grpc.status_code" => 0,
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      TestSupport.disconnect_client(channel)
    end
  end

  describe "error handling" do
    @status_cases [
      %{
        name: "TriggerOK",
        grpc_code: 0,
        expected_status: :undefined,
        extra_attrs: %{}
      },
      %{
        name: "TriggerCancelled",
        grpc_code: 1,
        expected_status: {:status, :error, "** (GRPC.RPCError) Client cancelled the request"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerUnknown",
        grpc_code: 2,
        expected_status: {:status, :error, "Unknown error"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerInvalidArgument",
        grpc_code: 3,
        expected_status: {:status, :error, "** (GRPC.RPCError) Invalid argument"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerDeadlineExceeded",
        grpc_code: 4,
        expected_status: {:status, :error, "Deadline exceeded"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerNotFound",
        grpc_code: 5,
        expected_status: {:status, :error, "** (GRPC.RPCError) Not found"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerAlreadyExists",
        grpc_code: 6,
        expected_status: {:status, :error, "** (GRPC.RPCError) Already exists"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerPermissionDenied",
        grpc_code: 7,
        expected_status: {:status, :error, "** (GRPC.RPCError) Permission denied"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerResourceExhausted",
        grpc_code: 8,
        expected_status: {:status, :error, "** (GRPC.RPCError) Resource exhausted"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerFailedPrecondition",
        grpc_code: 9,
        expected_status: {:status, :error, "** (GRPC.RPCError) Failed precondition"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerAborted",
        grpc_code: 10,
        expected_status: {:status, :error, "** (GRPC.RPCError) Aborted"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerOutOfRange",
        grpc_code: 11,
        expected_status: {:status, :error, "** (GRPC.RPCError) Out of range"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerUnimplemented",
        grpc_code: 12,
        expected_status: {:status, :error, "Unimplemented"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerInternal",
        grpc_code: 13,
        expected_status: {:status, :error, "Internal error"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerUnavailable",
        grpc_code: 14,
        expected_status: {:status, :error, "Service unavailable"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerDataLoss",
        grpc_code: 15,
        expected_status: {:status, :error, "Data loss"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      },
      %{
        name: "TriggerUnauthenticated",
        grpc_code: 16,
        expected_status: {:status, :error, "** (GRPC.RPCError) Unauthenticated"},
        extra_attrs: %{:"error.type" => "Elixir.GRPC.RPCError"}
      }
    ]

    for test_case <- @status_cases do
      @tag test_case: test_case
      test "#{test_case.name} records the proper #{inspect(test_case.expected_status)} status", %{
        port: port,
        test_case: test_case
      } do
        {:ok, channel} =
          TestSupport.connect_client(port,
            interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
          )

        TestService.Stub.say_hello(channel, %HelloRequest{name: test_case.name})

        assert_receive {:span,
                        span(
                          name: "testserver.v1.TestService/SayHello",
                          kind: :server,
                          status: status,
                          attributes: attributes
                        )}

        attr_map = :otel_attributes.map(attributes)

        assert status == test_case.expected_status

        # Build expected attributes
        expected_attrs = %{
          :"rpc.system" => :grpc,
          :"rpc.service" => "testserver.v1.TestService",
          :"rpc.method" => "SayHello",
          :"network.protocol.name" => "http",
          :"network.protocol.version" => "2",
          :"network.transport" => "tcp",
          :"rpc.grpc.status_code" => test_case.grpc_code,
          "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
        }

        assert attr_map == Map.merge(expected_attrs, test_case.extra_attrs)
      end
    end

    test "records span with exception", %{port: port} do
      {:ok, channel} =
        TestSupport.connect_client(port,
          interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
        )

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
               :"rpc.grpc.status_code" => 13,
               :"error.type" => "Elixir.RuntimeError",
               "elixir.grpc.server_module" => "Elixir.OpentelemetryGrpc.Test.TestServer"
             }

      assert status == OpenTelemetry.status(:error, "Server crashed")
    end
  end
end
