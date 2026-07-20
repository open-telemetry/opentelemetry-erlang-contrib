defmodule OpentelemetryCommanded.EventStoreTest do
  use OpentelemetryCommanded.CommandedCase, async: false

  alias OpentelemetryCommanded.DummyApp.Commands, as: C

  describe "dispatch command when Telemetry attached" do
    setup _ do
      case OpentelemetryCommanded.EventStore.setup() do
        :ok -> :ok
        {:error, :already_exists} -> :ok
      end
    end

    test "Success should create span", context do
      :ok = app_dispatch(context, %C.Ok{id: "ACC123"})

      assert_receive {:span,
                      span(
                        name: "commanded.event_store.ack_event",
                        kind: :internal,
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
               "commanded.stream_id": "ACC123",
               "commanded.stream_version": 1
             },
             attributes
           )
  end
end
