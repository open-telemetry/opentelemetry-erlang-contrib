defmodule OpentelemetryBroadwayRabbitMQ do
  def instrument(%Broadway.Message{metadata: metadata} = message, _opts) do
    case Map.fetch(metadata, :headers) do
      {:ok, amqp_headers} ->
        headers = Enum.map(amqp_headers, fn {key, _argument_type, value} -> {key, value} end)
        OpentelemetryBroadway.inject_into(message, headers)

      :error ->
        message
    end
  end
end
