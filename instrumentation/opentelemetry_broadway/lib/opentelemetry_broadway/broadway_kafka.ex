if Code.ensure_loaded?(BroadwayKafka) do
  defmodule OpentelemetryBroadwayKafka do
    def instrument(%Broadway.Message{metadata: metadata} = message, _opts) do
      OpentelemetryBroadway.inject_into(message, metadata[:headers])
    end

    defmodule Client do
      require OpenTelemetry.Tracer

      @behaviour BroadwayKafka.KafkaClient

      @impl BroadwayKafka.KafkaClient
      defdelegate init(opts), to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate setup(stage_pid, client_id, callback_module, config),
        to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate ack(group_coordinator, generation_id, topic, partition, offset, config),
        to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      def fetch(client_id, topic, partition, offset, opts, config) do
        attributes = receive_attributes(client_id, nil, topic, partition)

        OpenTelemetry.Tracer.with_span "#{topic} receive", kind: :consumer, attributes: attributes do
          BroadwayKafka.BrodClient.fetch(client_id, topic, partition, offset, opts, config)
        end
      end

      defp receive_attributes(client_id, consumer_group, topic, partition) do
        [
          "messaging.system": :kafka,
          "messaging.operation": :receive,
          "messaging.destination": topic,
          "messaging.destination_kind": :topic,
          "messaging.kafka.partition": partition,
          "messaging.kafka.consumer_group": consumer_group,
          "messaging.kafka.client_id": client_id
        ]
      end

      @impl BroadwayKafka.KafkaClient
      defdelegate resolve_offset(topic, partition, offset, offset_reset_policy, config),
        to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate update_topics(group_coordinator, topics), to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate connected?(client_id), to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate stop_group_coordinator(pid), to: BroadwayKafka.BrodClient

      @impl BroadwayKafka.KafkaClient
      defdelegate disconnect(client_id), to: BroadwayKafka.BrodClient
    end
  end
end
