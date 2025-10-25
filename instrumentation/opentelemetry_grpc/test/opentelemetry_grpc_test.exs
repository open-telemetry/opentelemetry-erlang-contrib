defmodule OpentelemetryGrpcTest do
  use ExUnit.Case

  @server_events [
    [:grpc, :server, :rpc, :start],
    [:grpc, :server, :rpc, :stop],
    [:grpc, :server, :rpc, :exception]
  ]

  @client_events [
    [:grpc, :client, :rpc, :start],
    [:grpc, :client, :rpc, :stop],
    [:grpc, :client, :rpc, :exception]
  ]

  @all_events @client_events ++ @server_events
  @server_start_event List.first(@server_events)

  doctest OpentelemetryGrpc

  describe "module structure" do
    test "all required modules exist" do
      assert Code.ensure_loaded?(OpentelemetryGrpc)
      assert Code.ensure_loaded?(OpentelemetryGrpc.Client)
      assert Code.ensure_loaded?(OpentelemetryGrpc.Server)
      assert Code.ensure_loaded?(OpentelemetryGrpc.ContextPropagatorInterceptor)
    end
  end

  describe "setup/1" do
    setup do
      for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)

      on_exit(fn ->
        for h <- :telemetry.list_handlers([]), do: :telemetry.detach(h.id)
      end)

      :ok
    end

    test "calling setup twice is safe (idempotent or explicit)" do
      :ok = OpentelemetryGrpc.setup()
      assert :ok = OpentelemetryGrpc.setup()

      assert handlers = :telemetry.list_handlers([])
      assert length(handlers) == length(@all_events)
    end

    test "sets up both client and server instrumentation by default" do
      :ok = OpentelemetryGrpc.setup()

      assert handlers = :telemetry.list_handlers([])

      for event <- @all_events do
        assert Enum.any?(handlers, &match?(%{event_name: ^event}, &1)),
               "Missing handler for event: #{inspect(event)}"
      end
    end

    test "can disable client instrumentation" do
      :ok = OpentelemetryGrpc.setup(client: :disabled)
      assert handlers = :telemetry.list_handlers([])

      for event <- @server_events do
        assert Enum.any?(handlers, &match?(%{event_name: ^event}, &1)),
               "Missing server handler for event: #{inspect(event)}"
      end

      for event <- @client_events do
        refute Enum.any?(handlers, &match?(%{event_name: ^event}, &1)),
               "Should not have client handler for event: #{inspect(event)}"
      end
    end

    test "can disable server instrumentation" do
      :ok = OpentelemetryGrpc.setup(server: :disabled)

      assert handlers = :telemetry.list_handlers([])

      for event <- @client_events do
        assert Enum.any?(handlers, &match?(%{event_name: ^event}, &1)),
               "Missing client handler for event: #{inspect(event)}"
      end

      for event <- @server_events do
        refute Enum.any?(handlers, &match?(%{event_name: ^event}, &1)),
               "Should not have server handler for event: #{inspect(event)}"
      end
    end

    test "can configure server span relationship" do
      :ok = OpentelemetryGrpc.setup(server: [span_relationship: :link])

      assert handlers = :telemetry.list_handlers([])

      server_start_handler =
        Enum.find(handlers, &match?(%{event_name: @server_start_event}, &1))

      assert server_start_handler
      assert server_start_handler.config.span_relationship == :link
    end

    test "can disable both client and server instrumentation" do
      :ok = OpentelemetryGrpc.setup(client: :disabled, server: :disabled)

      assert handlers = :telemetry.list_handlers([])

      grpc_handlers = Enum.filter(handlers, &match?(%{event_name: [:grpc | _]}, &1))

      assert grpc_handlers == []
    end

    test "raises on invalid server option" do
      assert_raise NimbleOptions.ValidationError, fn ->
        OpentelemetryGrpc.setup(server: :invalid)
      end
    end

    test "accepts valid client options" do
      :ok = OpentelemetryGrpc.setup()
      :ok = OpentelemetryGrpc.setup(client: :disabled)
    end

    test "raises on invalid client option" do
      assert_raise NimbleOptions.ValidationError, fn ->
        OpentelemetryGrpc.setup(client: :invalid)
      end

      assert_raise NimbleOptions.ValidationError, fn ->
        OpentelemetryGrpc.setup(client: true)
      end
    end
  end
end
