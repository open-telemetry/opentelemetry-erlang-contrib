defmodule OpentelemetryBroadwayKafkaTest do
  use ExUnit.Case, async: false

  import Record

  require Logger

  @moduletag :integration

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

  setup_all do
    {:ok, _} = Application.ensure_all_started(:broadway_kafka)

    :ok
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())
  end

  test "topology with instrumented kafka producer" do
    topic = "test"
    hosts = [localhost: 9092]

    reset_topic(hosts, topic)

    OpentelemetryBroadway.setup()

    OpentelemetryBroadway.start_link(Forwarder,
      name: MyBroadway,
      producer: [
        module:
          {BroadwayKafka.Producer,
           [
             hosts: hosts,
             group_id: "test_group",
             topics: [topic]
           ]},
        concurrency: 1
      ],
      processors: [default: [concurrency: 1]],
      batchers: [default: [concurrency: 1]]
    )

    wait_for_assignments(MyBroadway)
    send_messages(1, hosts, topic)

    assert_receive {:span, span(name: "producer", span_id: producer_span_id)}

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default receive",
                      span_id: processor_stage_span_id,
                      links: links
                    )},
                   :timer.seconds(5)

    assert [link(span_id: ^producer_span_id)] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Processor-default process",
                      parent_span_id: ^processor_stage_span_id,
                      links: links
                    )}

    assert [link(span_id: ^producer_span_id)] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-Batcher-default process",
                      links: links
                    )},
                   :timer.seconds(5)

    assert [link(span_id: ^producer_span_id)] = :otel_links.list(links)

    assert_receive {:span,
                    span(
                      name: "MyBroadway-BatchProcessor-default process",
                      links: links
                    )},
                   :timer.seconds(5)

    assert [link(span_id: ^producer_span_id)] = :otel_links.list(links)
  end

  defp reset_topic(hosts, topic) do
    :brod.delete_topics(hosts, [topic], 1_000)

    topic_config = [
      %{
        assignments: [],
        configs: [],
        name: topic,
        num_partitions: 3,
        replication_factor: 1
      }
    ]

    :brod.create_topics(hosts, topic_config, %{timeout: 1_000})
  end

  defp wait_for_assignments(broadway_name) do
    producers =
      broadway_name
      |> Broadway.producer_names()
      |> Enum.map(fn producer ->
        pid = Process.whereis(producer)
        :erlang.trace(pid, true, [:receive, tracer: self()])
        pid
      end)

    Enum.each(producers, fn pid ->
      receive do
        {:trace, ^pid, :receive, {:put_assignments, _, _}} ->
          Logger.info("Assignment received. Producer: #{inspect(pid)}")
      end
    end)
  end

  defp send_messages(n_messages, hosts, topic) do
    require OpenTelemetry.Tracer
    Logger.info("Sending messages...")

    client_id = :test_client
    :ok = :brod.start_client(hosts, client_id, _client_config = [])
    :ok = :brod.start_producer(client_id, topic, _producer_config = [])

    Enum.each(1..n_messages, fn i ->
      partition = rem(i, 3)

      OpenTelemetry.Tracer.with_span "producer", kind: :producer do
        headers = :otel_propagator_text_map.inject([])
        message = %{headers: headers, value: "#{i}"}

        :ok = :brod.produce_sync(client_id, topic, partition, _key = "", message)
      end
    end)

    :brod.stop_client(client_id)
  end
end
