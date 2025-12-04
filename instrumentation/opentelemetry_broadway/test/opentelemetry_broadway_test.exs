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

  # Basic Broadway instrumentation tests (without propagation)
  test "records span on successful message" do
    ref = Broadway.test_message(TestBroadway, "success")

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

    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("upstream-service")
      |> OpenTelemetry.Tracer.set_current_span()

    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    span_id = elem(trace_ctx, 2)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "upstream-service")}

    message = %Broadway.Message{
      data: "test message",
      metadata: %{
        headers: [
          {"traceparent", :longstr,
           "00-#{:io_lib.format("~32.16.0b", [trace_id])}-#{:io_lib.format("~16.16.0b", [span_id])}-01"},
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

    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":test_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.operation"] == :process

    links_list = elem(links, 5)
    assert length(links_list) == 1
    [link] = links_list
    assert elem(link, 1) == trace_id
    assert elem(link, 2) == span_id
  end

  test "creates proper trace relationship when propagation enabled" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    # Create parent trace
    parent_span = OpenTelemetry.Tracer.start_span("parent-service")
    OpenTelemetry.Tracer.set_current_span(parent_span)
    parent_ctx = OpenTelemetry.Tracer.current_span_ctx()
    parent_trace_id = elem(parent_ctx, 1)
    parent_span_id = elem(parent_ctx, 2)

    # Create traceparent header from parent context
    traceparent =
      "00-#{:io_lib.format("~32.16.0b", [parent_trace_id])}-#{:io_lib.format("~16.16.0b", [parent_span_id])}-01"

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    # Consume parent span
    assert_receive {:span, span(name: "parent-service", trace_id: ^parent_trace_id, span_id: ^parent_span_id)}

    # Create message with propagated context
    message = %Broadway.Message{
      data: "test",
      metadata: %{headers: [{"traceparent", :longstr, traceparent}]},
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    # Process the message
    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      %{
        processor_key: :default,
        topology_name: :test_topology,
        name: :"test_topology.Broadway.Consumer_0",
        message: message
      }
    )

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      %{message: %{message | status: :ok}}
    )

    # Verify the Broadway span
    assert_receive {:span,
                    span(
                      name: ":test_topology/default process",
                      trace_id: broadway_trace_id,
                      span_id: broadway_span_id,
                      links: links
                    )}

    # The Broadway span should have its own trace (not continue the parent trace)
    assert broadway_trace_id != parent_trace_id
    assert broadway_span_id != parent_span_id

    # But it should have a link to the parent
    links_list = elem(links, 5)
    assert length(links_list) == 1
    [link] = links_list
    # Link points to parent trace
    assert elem(link, 1) == parent_trace_id
    # Link points to parent span
    assert elem(link, 2) == parent_span_id
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

    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":test_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway

    links_list = elem(links, 5)
    assert length(links_list) == 0
  end

  test "handles malformed headers gracefully with propagation enabled" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    message = %Broadway.Message{
      data: "test message",
      metadata: %{
        headers: [
          {"traceparent", :longstr, "invalid-traceparent-format"},
          # Invalid key type
          {123, :longstr, "non-binary-key"},
          # Invalid value type
          {"valid-key", :longstr, 456},
          # Invalid header format
          nil
        ]
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

    # Should create a span without links since headers were malformed
    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":test_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway

    # No valid trace context should result in no links
    links_list = elem(links, 5)
    assert length(links_list) == 0
  end
end
