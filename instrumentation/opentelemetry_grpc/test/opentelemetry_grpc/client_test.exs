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

      assert status == :undefined

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

      assert status == :undefined

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
        expected_status: {:status, :error, "Client cancelled the request"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerUnknown",
        grpc_code: 2,
        expected_status: {:status, :error, "Unknown error"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerInvalidArgument",
        grpc_code: 3,
        expected_status: {:status, :error, "Invalid argument"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerDeadlineExceeded",
        grpc_code: 4,
        expected_status: {:status, :error, "Deadline exceeded"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerNotFound",
        grpc_code: 5,
        expected_status: {:status, :error, "Not found"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerAlreadyExists",
        grpc_code: 6,
        expected_status: {:status, :error, "Already exists"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerPermissionDenied",
        grpc_code: 7,
        expected_status: {:status, :error, "Permission denied"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerResourceExhausted",
        grpc_code: 8,
        expected_status: {:status, :error, "Resource exhausted"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerFailedPrecondition",
        grpc_code: 9,
        expected_status: {:status, :error, "Failed precondition"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerAborted",
        grpc_code: 10,
        expected_status: {:status, :error, "Aborted"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerOutOfRange",
        grpc_code: 11,
        expected_status: {:status, :error, "Out of range"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerUnimplemented",
        grpc_code: 12,
        expected_status: {:status, :error, "Unimplemented"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerInternal",
        grpc_code: 13,
        expected_status: {:status, :error, "Internal error"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerUnavailable",
        grpc_code: 14,
        expected_status: {:status, :error, "Service unavailable"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerDataLoss",
        grpc_code: 15,
        expected_status: {:status, :error, "Data loss"},
        extra_attrs: %{:"error.type" => "grpc_error"}
      },
      %{
        name: "TriggerUnauthenticated",
        grpc_code: 16,
        expected_status: {:status, :error, "Unauthenticated"},
        extra_attrs: %{:"error.type" => "grpc_error"}
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
                          kind: :client,
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
          :"server.address" => "127.0.0.1",
          :"server.port" => port,
          :"rpc.grpc.status_code" => test_case.grpc_code
        }

        assert attr_map == Map.merge(expected_attrs, test_case.extra_attrs)

        TestSupport.disconnect_client(channel)
      end
    end
  end
end
