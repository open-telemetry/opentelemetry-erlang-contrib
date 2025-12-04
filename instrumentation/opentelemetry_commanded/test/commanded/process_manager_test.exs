defmodule OpentelemetryCommanded.ProcessManagerTest do
  use OpentelemetryCommanded.CommandedCase, async: false

  import ExUnit.CaptureLog

  alias OpentelemetryCommanded.DummyApp.Commands, as: C
  alias OpentelemetryCommanded.DummyApp.Events, as: E

  describe "dispatch command when Telemetry attached" do
    setup _ do
      case OpentelemetryCommanded.ProcessManager.setup() do
        :ok -> :ok
        {:error, :already_exists} -> :ok
      end
    end

    test "Success should create span", context do
      :ok = app_dispatch(context, %C.Ok{id: "ACC123"})

      assert_receive {:span,
                      span(
                        name: "commanded.process_manager.handle",
                        kind: :consumer,
                        parent_span_id: parent_span_id,
                        attributes: attributes
                      )}

      # Get parent span to ensure context has been propagated across the process
      assert_receive {:span, span(name: parent_span_name, span_id: ^parent_span_id)}

      assert parent_span_name in [
               "opentelemetry_commanded.test",
               "commanded.application.dispatch"
             ]

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{
                 "commanded.event": "Elixir.OpentelemetryCommanded.DummyApp.Events.OkEvent"
               },
               attributes
             )
    end

    test "Error should create span with error set", context do
      _log =
        capture_log(fn ->
          :ok =
            app_dispatch(context, %C.DoEvent{
              id: "ACC123",
              event: %E.ErrorInProcessManagerEvent{id: "ACC123", message: "some error"}
            })
        end)

      assert_receive {:span,
                      span(
                        name: "commanded.process_manager.handle",
                        kind: :consumer,
                        status: {:status, :error, exception_message},
                        attributes: attributes
                      )}

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{
                 "commanded.event":
                   "Elixir.OpentelemetryCommanded.DummyApp.Events.ErrorInProcessManagerEvent"
               },
               attributes
             )

      assert exception_message =~ "some error"
    end

    test "Exception should create span with error set", context do
      _log =
        capture_log(fn ->
          :ok =
            app_dispatch(context, %C.DoEvent{
              id: "ACC123",
              event: %E.ExceptionInProcessManagerEvent{id: "ACC123", message: "some error"}
            })
        end)

      assert_receive {:span,
                      span(
                        name: "commanded.process_manager.handle",
                        kind: :consumer,
                        status: {:status, :error, exception_message},
                        attributes: attributes,
                        events: events
                      )}

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{
                 "commanded.event":
                   "Elixir.OpentelemetryCommanded.DummyApp.Events.ExceptionInProcessManagerEvent"
               },
               attributes
             )

      assert exception_message =~ "some error"

      [
        event(
          name: "exception",
          attributes: event_attributes
        )
      ] = :otel_events.list(events)

      event_attributes = :otel_attributes.map(event_attributes)

      assert match?(
               %{
                 "exception.message" => "some error",
                 "exception.stacktrace" => _,
                 "exception.type" => "Elixir.RuntimeError"
               },
               event_attributes
             )

      stack_trace = event_attributes["exception.stacktrace"]
      assert stack_trace =~ "OpentelemetryCommanded.DummyApp.ProcessManager.handle/2"
    end
  end

  defp has_basic_attributes!(attributes, correlation_id) do
    assert match?(
             %{
               "commanded.application": OpentelemetryCommanded.DummyApp.App,
               "commanded.causation_id": _,
               "commanded.correlation_id": ^correlation_id,
               "commanded.event": _,
               "commanded.event_id": _,
               "commanded.event_number": 1,
               "commanded.handler_name": "ProcessManager",
               "commanded.process_uuid": "ACC123",
               "commanded.stream_id": "ACC123",
               "commanded.stream_version": 1,
               "messaging.conversation_id": ^correlation_id,
               "messaging.destination": OpentelemetryCommanded.DummyApp.ProcessManager,
               "messaging.destination_kind": "process_manager",
               "messaging.message_id": _,
               "messaging.operation": "receive",
               "messaging.system": "commanded"
             },
             attributes
           )
  end
end
