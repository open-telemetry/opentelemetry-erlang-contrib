defmodule OpentelemetryBroadwayTest do
  use ExUnit.Case
  doctest OpentelemetryBroadway

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup()

    :ok
  end

  test "records span on succesful message" do
    ref = Broadway.test_message(TestBroadway, "success")

    #  Confirm the message was processed
    assert_receive {:ack, ^ref, [%{data: "success"}], []}

    expected_status = OpenTelemetry.status(:ok)

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default process",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :consumer,
                      status: ^expected_status
                    )}

    assert %{
             "messaging.system": :broadway,
             "messaging.operation": :process,
             "messaging.message_payload_size_bytes": 7
           } = :otel_attributes.map(attributes)
  end

  test "records span on message which fails" do
    ref = Broadway.test_message(TestBroadway, "error")

    #  Confirm the message was processed
    assert_receive {:ack, ^ref, [], [%{data: "error"}]}

    expected_status = OpenTelemetry.status(:error, "something went wrong")

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default process",
                      parent_span_id: :undefined,
                      kind: :consumer,
                      status: ^expected_status
                    )}
  end

  test "records span on an exception being thrown" do
    ref = Broadway.test_message(TestBroadway, "exception")

    #  Confirm the message was processed
    assert_receive {:ack, ^ref, [], [%{data: "exception"}]}

    expected_status = OpenTelemetry.status(:error, "** (RuntimeError) an exception occurred")

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default process",
                      parent_span_id: :undefined,
                      kind: :consumer,
                      status: ^expected_status
                    )}
  end

  test "records span on successful batch" do
    ref = Broadway.test_batch(TestBroadway, ["batch success", "batch success"])

    assert_receive {:ack, ^ref, [%{data: "batch success"}, %{data: "batch success"}], []}

    expected_status = OpenTelemetry.status(:ok, "")

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default batching process",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :internal,
                      status: ^expected_status
                    )}

    assert %{
              "broadway.batch.count": 2,
              "broadway.batch.failed.count": 0,
              "broadway.batch.success.count": 2
            } = :otel_attributes.map(attributes)
  end

  test "records span on batch with exception" do
    ref = Broadway.test_batch(TestBroadway, ["batch success", "batch exception"])

    # Uncaught exceptions cause Broadway to fail the whole batch
    assert_receive {:ack, ^ref, [], [%{data: "batch success"}, %{data: "batch exception"}]}

    expected_status = OpenTelemetry.status(:error, "Batch completed with failed messages")

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default batching process",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :internal,
                      status: ^expected_status
                    )}

    assert %{
              "broadway.batch.count": 2,
              "broadway.batch.failed.count": 2,
              "broadway.batch.success.count": 0
            } = :otel_attributes.map(attributes)
  end

  test "records span on batch with failed messages" do
    ref = Broadway.test_batch(TestBroadway, ["batch success", "batch error"])

    assert_receive {:ack, ^ref, [%{data: "batch success"}], [%{data: "batch error"}]}

    expected_status = OpenTelemetry.status(:error, "Batch completed with failed messages")

    assert_receive {:span,
                    span(
                      name: "TestBroadway/default batching process",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :internal,
                      status: ^expected_status
                    )}

    assert %{
              "broadway.batch.count": 2,
              "broadway.batch.failed.count": 1,
              "broadway.batch.success.count": 1
            } = :otel_attributes.map(attributes)
  end
end
