defmodule OpentelemetryCommanded.ApplicationTest do
  use OpentelemetryCommanded.CommandedCase, async: false

  alias OpentelemetryCommanded.DummyApp
  alias OpentelemetryCommanded.DummyApp.Commands, as: C

  import ExUnit.CaptureLog
  require Logger

  describe "dispatch command when telemetry attached" do
    setup _ do
      case OpentelemetryCommanded.Application.setup() do
        :ok -> :ok
        {:error, :already_exists} -> :ok
      end
    end

    test "Success should create span", context do
      :ok = app_dispatch(context, %C.Ok{id: "ACC123"})

      assert_receive {:span,
                      span(
                        name: "commanded.application.dispatch",
                        kind: :consumer,
                        parent_span_id: parent_span_id,
                        attributes: attributes
                      )}

      # Get parent span to ensure context has been propagated across the process
      assert_receive {:span, span(name: parent_span_name, span_id: ^parent_span_id)}
      assert parent_span_name in ["opentelemetry_commanded.test"]

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{"commanded.command": DummyApp.Commands.Ok},
               attributes
             )
    end

    test "when error create span with error status", context do
      # TODO: this needs to be done better when Commanded Application dispatch spans fixed
      {:error, "some error"} =
        app_dispatch(
          context,
          %C.Error{id: Ecto.UUID.generate(), message: "some error"}
        )

      assert_receive {:span,
                      span(
                        name: "commanded.application.dispatch",
                        kind: :consumer,
                        status: {:status, :error, "\"some error\""},
                        attributes: attributes
                      )}

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{"commanded.command": DummyApp.Commands.Error},
               attributes
             )
    end

    test "when no handler for Command, create span with error status", context do
      captured_log =
        capture_log(fn ->
          {:error,
           %FunctionClauseError{
             args: nil,
             arity: 2,
             clauses: nil,
             function: :handle,
             kind: nil,
             module: OpentelemetryCommanded.DummyApp.Handler
           }} =
            app_dispatch(context, %C.RaiseException{
              id: Ecto.UUID.generate(),
              message: "no handler"
            })
        end)

      assert captured_log =~ "%FunctionClauseError{"
      assert captured_log =~ "module: OpentelemetryCommanded.DummyApp.Handler"
      assert captured_log =~ "function: :handle"
      assert captured_log =~ "arity: 2"

      assert_receive {:span,
                      span(
                        name: "commanded.application.dispatch",
                        kind: :consumer,
                        status: {:status, :error, exception_message},
                        attributes: attributes
                      )}

      assert exception_message =~ "%FunctionClauseError{"
      assert exception_message =~ "module: OpentelemetryCommanded.DummyApp.Handler"
      assert exception_message =~ "function: :handle"
      assert exception_message =~ "arity: 2"

      attributes = :otel_attributes.map(attributes)

      has_basic_attributes!(attributes, context.correlation_id)

      assert match?(
               %{"commanded.command": DummyApp.Commands.RaiseException},
               attributes
             )
    end
  end

  defp has_basic_attributes!(attributes, correlation_id) do
    assert match?(
             %{
               "commanded.application": DummyApp.App,
               "commanded.function": :handle,
               "messaging.conversation_id": ^correlation_id,
               "messaging.destination": DummyApp.Handler,
               "messaging.destination_kind": "command_handler",
               "messaging.message_id": _,
               "messaging.operation": "receive",
               "messaging.system": "commanded"
             },
             attributes
           )
  end
end
