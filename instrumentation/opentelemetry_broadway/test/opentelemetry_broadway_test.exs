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

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.operation.type"] == :process
    assert attrs_map[:"messaging.message.body.size"] == 7
  end

  test "records span on successful batch processor event" do
    messages = [
      %Broadway.Message{data: "first", metadata: %{}, acknowledger: {Broadway.NoopAcknowledger, nil, nil}},
      %Broadway.Message{data: "second", metadata: %{}, acknowledger: {Broadway.NoopAcknowledger, nil, nil}}
    ]

    :telemetry.execute(
      [:broadway, :batch_processor, :start],
      %{},
      %{
        topology_name: :test_topology,
        name: :"test_topology.Broadway.BatchProcessor_0",
        messages: messages,
        batch_info: %{batcher: :default}
      }
    )

    :telemetry.execute(
      [:broadway, :batch_processor, :stop],
      %{},
      %{successful_messages: messages, failed_messages: []}
    )

    expected_status = OpenTelemetry.status(:ok)

    assert_receive {:span,
                    span(
                      name: ":test_topology/default batch",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :consumer,
                      status: ^expected_status
                    )}

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.operation.type"] == :process
    assert attrs_map[:"messaging.batch.message_count"] == 2
    assert attrs_map[:"broadway.messaging.batch.successful_count"] == 2
    assert attrs_map[:"broadway.messaging.batch.failed_count"] == 0
  end

  test "records batch span for a real Broadway batch" do
    ref = Broadway.test_batch(TestBroadway, ["success", "success"], batch_mode: :bulk)

    assert_receive {:ack, ^ref, successful, []}, 1_000
    assert length(successful) == 2

    span = assert_receive_span_named("TestBroadway/default batch")
    attrs_map = :otel_attributes.map(span(span, :attributes))

    assert span(span, :kind) == :consumer
    assert span(span, :status) == OpenTelemetry.status(:ok)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.operation.type"] == :process
    assert attrs_map[:"messaging.batch.message_count"] == 2
    assert attrs_map[:"broadway.messaging.batch.successful_count"] == 2
    assert attrs_map[:"broadway.messaging.batch.failed_count"] == 0
  end

  test "marks batch processor span as errored when failed messages are present" do
    messages = [
      %Broadway.Message{data: "first", metadata: %{}, acknowledger: {Broadway.NoopAcknowledger, nil, nil}},
      %Broadway.Message{data: "second", metadata: %{}, acknowledger: {Broadway.NoopAcknowledger, nil, nil}}
    ]

    :telemetry.execute(
      [:broadway, :batch_processor, :start],
      %{},
      %{
        topology_name: :test_topology,
        name: :"test_topology.Broadway.BatchProcessor_0",
        messages: messages,
        batch_info: %{batcher: :default}
      }
    )

    :telemetry.execute(
      [:broadway, :batch_processor, :stop],
      %{},
      %{successful_messages: [hd(messages)], failed_messages: [List.last(messages)]}
    )

    expected_status = OpenTelemetry.status(:error, "1 messages failed")

    assert_receive {:span,
                    span(
                      name: ":test_topology/default batch",
                      attributes: attributes,
                      kind: :consumer,
                      status: ^expected_status
                    )}

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"broadway.messaging.batch.successful_count"] == 1
    assert attrs_map[:"broadway.messaging.batch.failed_count"] == 1
  end

  test "collects unique propagated links for batch processor spans" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("batch-upstream-service")
      |> OpenTelemetry.Tracer.set_current_span()

    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    hex_trace_id = elem(trace_ctx, 2)
    span_id = elem(trace_ctx, 3)
    hex_span_id = elem(trace_ctx, 4)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "batch-upstream-service")}

    traceparent = "00-#{hex_trace_id}-#{hex_span_id}-01"

    messages = [
      %Broadway.Message{
        data: "first",
        metadata: %{headers: [{"traceparent", :longstr, traceparent}]},
        acknowledger: {Broadway.NoopAcknowledger, nil, nil}
      },
      %Broadway.Message{
        data: "second",
        metadata: %{headers: [{"traceparent", :longstr, traceparent}]},
        acknowledger: {Broadway.NoopAcknowledger, nil, nil}
      }
    ]

    :telemetry.execute(
      [:broadway, :batch_processor, :start],
      %{},
      %{
        topology_name: :test_topology,
        name: :"test_topology.Broadway.BatchProcessor_0",
        messages: messages,
        batch_info: %{batcher: :default}
      }
    )

    :telemetry.execute(
      [:broadway, :batch_processor, :stop],
      %{},
      %{successful_messages: messages, failed_messages: []}
    )

    assert_receive {:span, span(name: ":test_topology/default batch", links: links)}

    links_list = elem(links, 5)
    assert length(links_list) == 1
    [link] = links_list
    assert elem(link, 1) == trace_id
    assert elem(link, 2) == span_id
  end

  test "batch processor span has no links when propagation is disabled" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: false)

    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("batch-upstream-disabled")
      |> OpenTelemetry.Tracer.set_current_span()

    traceparent = create_rabbitmq_traceparent()

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "batch-upstream-disabled")}

    messages = [
      %Broadway.Message{
        data: "success",
        metadata: %{headers: [{"traceparent", :longstr, traceparent}]},
        acknowledger: {Broadway.NoopAcknowledger, nil, nil}
      }
    ]

    :telemetry.execute(
      [:broadway, :batch_processor, :start],
      %{},
      %{
        topology_name: :test_topology,
        name: :"test_topology.Broadway.BatchProcessor_0",
        messages: messages,
        batch_info: %{batcher: :default}
      }
    )

    :telemetry.execute(
      [:broadway, :batch_processor, :stop],
      %{},
      %{successful_messages: messages, failed_messages: []}
    )

    assert_receive {:span, span(name: ":test_topology/default batch", links: links)}

    links_list = elem(links, 5)
    assert length(links_list) == 0
  end

  test "batch processor spans use links when span_relationship is child and messages have multiple parents" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(span_relationship: :child)

    {first_trace_id, first_span_id, first_traceparent} = create_parent_traceparent("batch-parent-one")
    {second_trace_id, second_span_id, second_traceparent} = create_parent_traceparent("batch-parent-two")

    messages = [
      %Broadway.Message{
        data: "first",
        metadata: %{headers: [{"traceparent", :longstr, first_traceparent}]},
        acknowledger: {Broadway.NoopAcknowledger, nil, nil}
      },
      %Broadway.Message{
        data: "second",
        metadata: %{headers: [{"traceparent", :longstr, second_traceparent}]},
        acknowledger: {Broadway.NoopAcknowledger, nil, nil}
      }
    ]

    :telemetry.execute(
      [:broadway, :batch_processor, :start],
      %{},
      %{
        topology_name: :test_topology,
        name: :"test_topology.Broadway.BatchProcessor_0",
        messages: messages,
        batch_info: %{batcher: :default}
      }
    )

    :telemetry.execute(
      [:broadway, :batch_processor, :stop],
      %{},
      %{successful_messages: messages, failed_messages: []}
    )

    assert_receive {:span, span(name: ":test_topology/default batch", trace_id: batch_trace_id, links: links)}

    links_list = elem(links, 5)
    assert length(links_list) == 2

    link_pairs =
      links_list
      |> Enum.map(fn link -> {elem(link, 1), elem(link, 2)} end)
      |> MapSet.new()

    assert MapSet.member?(link_pairs, {first_trace_id, first_span_id})
    assert MapSet.member?(link_pairs, {second_trace_id, second_span_id})
    refute batch_trace_id == first_trace_id
    refute batch_trace_id == second_trace_id
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
    hex_trace_id = elem(trace_ctx, 2)
    span_id = elem(trace_ctx, 3)
    hex_span_id = elem(trace_ctx, 4)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "upstream-service")}

    message = %Broadway.Message{
      data: "test message",
      metadata: %{
        headers: [
          {"traceparent", :longstr, "00-#{hex_trace_id}-#{hex_span_id}-01"},
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
    assert attrs_map[:"messaging.operation.type"] == :process
    assert attrs_map[:"messaging.message.body.size"] == byte_size("test message")
    assert attrs_map[:"messaging.message.id"] == "123"

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
    hex_parent_trace_id = elem(parent_ctx, 2)
    parent_span_id = elem(parent_ctx, 3)
    hex_parent_span_id = elem(parent_ctx, 4)

    # Create traceparent header from parent context
    traceparent = "00-#{hex_parent_trace_id}-#{hex_parent_span_id}-01"

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

  test "handles SQS metadata with empty list attributes" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    # SQS returns empty lists [] instead of empty maps %{} when no attributes exist
    message = %Broadway.Message{
      data: "sqs message",
      metadata: %{
        attributes: [],
        message_id: "a59662d4-76c5-43f7-8a80-5f9749038741",
        message_attributes: [],
        md5_of_body: "3fbc4efbd123b952ace9b008b844b800",
        receipt_handle: "AQEBzefW2cSz0tC2w7trdXpCSqQWD5zieIGDLzLU4aRS..."
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :sqs_topology,
      name: :"sqs_topology.Broadway.Consumer_0",
      message: message
    }

    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    completed_message = %{message | status: :ok}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      %{message: completed_message}
    )

    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":sqs_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway
    assert attrs_map[:"messaging.message.id"] == "a59662d4-76c5-43f7-8a80-5f9749038741"

    links_list = elem(links, 5)
    assert length(links_list) == 0
  end

  test "extracts trace context from SQS message_attributes" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("sqs-producer")
      |> OpenTelemetry.Tracer.set_current_span()

    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    hex_trace_id = elem(trace_ctx, 2)
    span_id = elem(trace_ctx, 3)
    hex_span_id = elem(trace_ctx, 4)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "sqs-producer")}

    traceparent = "00-#{hex_trace_id}-#{hex_span_id}-01"

    # Real SQS metadata structure from ExAws.SQS with all message attribute types:
    # - String: regular string values
    # - String.custom: string with custom type suffix
    # - Number: numeric values (stored as string_value)
    # - Binary: binary data (base64 encoded in binary_value)
    message = %Broadway.Message{
      data: "sqs message with trace",
      metadata: %{
        attributes: %{
          "ApproximateFirstReceiveTimestamp" => 1_700_000_000_000,
          "ApproximateReceiveCount" => 1,
          "SenderId" => "AROAEXAMPLEID:test-service",
          "SentTimestamp" => 1_700_000_000_000
        },
        message_id: "734ec1f6-479c-4add-b369-8cfdf1b20be0",
        md5_of_body: "c08bbff48213759b9cd06a03f9abe844",
        message_attributes: %{
          # W3C Trace Context header for propagation
          "traceparent" => %{
            name: "traceparent",
            data_type: "String",
            value: traceparent,
            string_value: traceparent,
            binary_value: ""
          },
          # String with custom type suffix
          "EventType" => %{
            name: "EventType",
            data_type: "String.application/json",
            value: "order.created",
            string_value: "order.created",
            binary_value: ""
          },
          # Number type (value is parsed, string_value has original)
          "RetryCount" => %{
            name: "RetryCount",
            data_type: "Number",
            value: 3,
            string_value: "3",
            binary_value: ""
          },
          # Binary type (base64 encoded)
          "Signature" => %{
            name: "Signature",
            data_type: "Binary",
            value: "binarydata",
            string_value: "",
            binary_value: "YmluYXJ5ZGF0YQ=="
          }
        },
        receipt_handle: "AQEBVeGALM2wkdaRfuNjxHDJOf0ZtflVnwvNB9hNL8..."
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :sqs_topology,
      name: :"sqs_topology.Broadway.Consumer_0",
      message: message
    }

    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    completed_message = %{message | status: :ok}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      %{message: completed_message}
    )

    assert_receive {:span, span(name: span_name, links: links)}

    assert span_name == ":sqs_topology/default process"

    links_list = elem(links, 5)
    assert length(links_list) == 1
    [link] = links_list
    assert elem(link, 1) == trace_id
    assert elem(link, 2) == span_id
  end

  test "handles PubSub metadata with nil attributes" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    # PubSub sets attributes to nil when no attributes are present on the message
    message = %Broadway.Message{
      data: "pubsub message",
      metadata: %{
        attributes: nil,
        message_id: "1234567890",
        ordering_key: "",
        publish_time: ~U[2024-01-01 00:00:00Z]
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :pubsub_topology,
      name: :"pubsub_topology.Broadway.Consumer_0",
      message: message
    }

    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    completed_message = %{message | status: :ok}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      %{message: completed_message}
    )

    assert_receive {:span, span(name: span_name, attributes: attributes, links: links)}

    assert span_name == ":pubsub_topology/default process"

    attrs_map = :otel_attributes.map(attributes)
    assert attrs_map[:"messaging.system"] == :broadway

    links_list = elem(links, 5)
    assert length(links_list) == 0
  end

  test "extracts trace context from PubSub attributes" do
    TestHelpers.remove_handlers()
    :ok = OpentelemetryBroadway.setup(propagation: true)

    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span("pubsub-producer")
      |> OpenTelemetry.Tracer.set_current_span()

    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    hex_trace_id = elem(trace_ctx, 2)
    span_id = elem(trace_ctx, 3)
    hex_span_id = elem(trace_ctx, 4)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: "pubsub-producer")}

    traceparent = "00-#{hex_trace_id}-#{hex_span_id}-01"

    # Real PubSub metadata structure from broadway_cloud_pub_sub:
    # - attributes is a plain string-to-string map
    message = %Broadway.Message{
      data: "pubsub message with trace",
      metadata: %{
        attributes: %{
          "traceparent" => traceparent,
          "custom-header" => "custom-value"
        },
        message_id: "9876543210",
        ordering_key: "",
        publish_time: ~U[2024-01-01 00:00:00Z]
      },
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }

    start_metadata = %{
      processor_key: :default,
      topology_name: :pubsub_topology,
      name: :"pubsub_topology.Broadway.Consumer_0",
      message: message
    }

    :telemetry.execute(
      [:broadway, :processor, :message, :start],
      %{},
      start_metadata
    )

    completed_message = %{message | status: :ok}

    :telemetry.execute(
      [:broadway, :processor, :message, :stop],
      %{},
      %{message: completed_message}
    )

    assert_receive {:span, span(name: span_name, links: links)}

    assert span_name == ":pubsub_topology/default process"

    links_list = elem(links, 5)
    assert length(links_list) == 1
    [link] = links_list
    assert elem(link, 1) == trace_id
    assert elem(link, 2) == span_id
  end

  # Backwards compatibility tests for deprecated `propagation` option
  describe "backwards compatibility" do
    test "propagation: false disables context propagation" do
      TestHelpers.remove_handlers()
      :ok = OpentelemetryBroadway.setup(propagation: false)

      _parent_span_ctx =
        OpenTelemetry.Tracer.start_span("upstream-service")
        |> OpenTelemetry.Tracer.set_current_span()

      # Create headers with trace context
      headers = create_rabbitmq_headers_with_trace_context()

      ref =
        Broadway.test_message(TestBroadway, "success", metadata: %{headers: headers, delivery_tag: 12345})

      assert_receive {:ack, ^ref, [%{data: "success"}], []}

      # Should not have parent-child relationship when propagation: false
      assert_receive {:span, span(parent_span_id: :undefined, links: links)}

      # Should not have any links when propagation: false
      links_list = elem(links, 5)
      assert length(links_list) == 0
    end

    test "raises error when both propagation and span_relationship are provided" do
      TestHelpers.remove_handlers()

      assert_raise ArgumentError,
                   "cannot use both :propagation and :span_relationship options. " <>
                     "Please use :span_relationship only as :propagation is deprecated",
                   fn ->
                     OpentelemetryBroadway.setup(propagation: true, span_relationship: :none)
                   end
    end
  end

  defp create_rabbitmq_headers_with_trace_context do
    [{"traceparent", :longstr, create_rabbitmq_traceparent()}]
  end

  defp create_rabbitmq_traceparent do
    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    hex_trace_id = elem(trace_ctx, 2)
    hex_span_id = elem(trace_ctx, 4)

    "00-#{hex_trace_id}-#{hex_span_id}-01"
  end

  defp create_parent_traceparent(name) do
    _parent_span_ctx =
      OpenTelemetry.Tracer.start_span(name)
      |> OpenTelemetry.Tracer.set_current_span()

    trace_ctx = OpenTelemetry.Tracer.current_span_ctx()
    trace_id = elem(trace_ctx, 1)
    hex_trace_id = elem(trace_ctx, 2)
    span_id = elem(trace_ctx, 3)
    hex_span_id = elem(trace_ctx, 4)

    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Ctx.clear()

    assert_receive {:span, span(name: ^name)}

    {trace_id, span_id, "00-#{hex_trace_id}-#{hex_span_id}-01"}
  end

  defp assert_receive_span_named(name, timeout \\ 1_000) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_assert_receive_span_named(name, deadline)
  end

  defp do_assert_receive_span_named(name, deadline) do
    remaining = max(deadline - System.monotonic_time(:millisecond), 0)

    receive do
      {:span, span(name: ^name) = span} ->
        span

      {:span, _other_span} ->
        do_assert_receive_span_named(name, deadline)
    after
      remaining ->
        flunk("expected span named #{inspect(name)}")
    end
  end
end
