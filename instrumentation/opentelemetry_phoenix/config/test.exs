config :opentelemetry,
  processors: [{:otel_batch_processor, %{scheduled_delay_ms: 1}}]
