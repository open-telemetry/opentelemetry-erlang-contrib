defmodule OpentelemetryBroadwayTest do
  use ExUnit.Case, async: false

  import Record

  require Logger
  require OpenTelemetry.Span
  require OpenTelemetry.Tracer

  doctest OpentelemetryBroadway

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    defrecord(name, spec)
  end

  defmodule Forwarder do
    use Broadway

    def handle_message(:default, message, _context) do
      message
    end

    def handle_batch(:default, messages, _batch_info, _context) do
      messages
    end
  end

  defmodule Propagator do
    use Broadway

    def handle_message(:default, message, _context) do
      message
      |> OpentelemetryBroadway.propagate()
    end

    def handle_batch(:default, messages, _batch_info, _context) do
      messages
    end
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())
  end

  test "topology stage spans and attributes" do
    OpentelemetryBroadway.setup()

    Broadway.start_link(Forwarder,
      name: MyBroadway,
      producer: [module: {Broadway.DummyProducer, []}, concurrency: 1],
      processors: [default: [concurrency: 1]],
      batchers: [default: [concurrency: 1]]
    )

    ref = Broadway.test_message(MyBroadway, 1)
    assert_receive {:ack, ^ref, [%{data: 1}], []}

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default process",
                      kind: :internal,
                      span_id: processor_stage_span_id,
                      links: links,
                      attributes: attributes
                    )}

    assert [] = :otel_links.list(links)

    assert %{
             "broadway.topology_name": "MyBroadway",
             "broadway.stage": :processor,
             "broadway.index": 0,
             "broadway.processor_key": :default,
             "broadway.messages_count": 1
           } = :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default message",
                      kind: :internal,
                      parent_span_id: ^processor_stage_span_id,
                      links: links,
                      attributes: attributes
                    )}

    assert [] = :otel_links.list(links)

    assert %{} = :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Batcher-default process",
                      kind: :internal,
                      links: links,
                      attributes: attributes
                    )}

    assert [] = :otel_links.list(links)

    assert %{
             "broadway.topology_name": "MyBroadway",
             "broadway.stage": :batcher,
             "broadway.batch_key": :default,
             "broadway.messages_count": 1
           } = :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-BatchProcessor-default process",
                      kind: :internal,
                      links: links,
                      attributes: attributes
                    )}

    assert [] = :otel_links.list(links)

    assert %{
             "broadway.topology_name": "MyBroadway",
             "broadway.stage": :batch_processor,
             "broadway.index": 0,
             "broadway.batch_key": ":default",
             "broadway.batch_size": 1,
             "broadway.successful_messages_count": 1,
             "broadway.failed_messages_count": 0
           } = :otel_attributes.map(attributes)
  end

  test "topology with propagator at processor produces links on batcher" do
    OpentelemetryBroadway.setup()

    Broadway.start_link(Propagator,
      name: MyBroadway,
      producer: [module: {Broadway.DummyProducer, []}, concurrency: 1],
      processors: [default: [concurrency: 1]],
      batchers: [default: [concurrency: 1]]
    )

    ref = Broadway.test_message(MyBroadway, 1)
    assert_receive {:ack, ^ref, [%{data: 1}], []}

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default process",
                      span_id: processor_stage_span_id,
                      links: links
                    )}

    assert [] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default message",
                      parent_span_id: ^processor_stage_span_id,
                      span_id: processor_message_span_id,
                      links: links
                    )}

    assert [] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Batcher-default process",
                      links: links
                    )}

    assert [link(span_id: ^processor_message_span_id)] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-BatchProcessor-default process",
                      links: links
                    )}

    assert [link(span_id: ^processor_message_span_id)] = :otel_links.list(links)
  end
end
