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

    #  Confirm the message was processed
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

  test "extracts trace context from RabbitMQ headers when propagation enabled" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    # Create a parent span to simulate upstream service
    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("upstream-service")
      |> OpenTelemetry.Tracer.set_current_span()

    # Extract trace context to simulate what would be in headers
    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    span_id = elem(trace_ctx, 2)

    # End parent span and clear context
    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    # Consume the upstream span
    assert_receive {:span, span(name: "upstream-service")}

    # Create a message with RabbitMQ headers containing trace context
    message = %Broadway.Message{
      data: "test message",
      metadata: %{
        headers: [
          {"traceparent", :longstr, "00-#{:io_lib.format("~32.16.0b", [trace_id])}-#{:io_lib.format("~16.16.0b", [span_id])}-01"},
          {"x-custom-header", :longstr, "custom-value"}
        ],
        routing_key: "test.queue",
        exchange: "test.exchange",
        delivery_tag: 123,
        correlation_id: "correlation-123"
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :test_topology,
      name: :"test_topology.Broadway.Consumer_0",
      message: message
    }

    # Simulate message start
    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    # Simulate message completion
    completed_message = %{message | status: :ok}
    stop_metadata = %{message: completed_message}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      stop_metadata
    )

    # Verify span was created with trace propagation
    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":test_topology/default process"

    # Verify basic attributes (no system-specific ones)
    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.operation"] == :process

    # Verify trace propagation created a link
    # Links are stored as a record, need to extract the list
    links_list = elem(links, 5) # Get the actual links list from the record
    assert length(links_list) == 1
    [link] = links_list
    # Link is a record: {:link, trace_id, span_id, attributes, tracestate}
    assert elem(link, 1) == trace_id  # trace_id is at position 1
    assert elem(link, 2) == span_id   # span_id is at position 2
  end

  test "handles message without headers gracefully with propagation enabled" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    message = %Broadway.Message{
      data: "test message",
      metadata: %{
        routing_key: "test.queue"
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :test_topology,
      name: :"test_topology.Broadway.Consumer_0",
      message: message
    }

    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    completed_message = %{message | status: :ok}
    stop_metadata = %{message: completed_message}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      stop_metadata
    )

    # Verify span was created without links
    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":test_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway

    # Verify no links were created
    links_list = elem(links, 5)
    assert length(links_list) == 0
  end
end
