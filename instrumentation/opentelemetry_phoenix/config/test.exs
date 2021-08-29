config :opentelemetry,
  sampler: {:always_on, %{}},
  tracer: :ot_tracer_default,
  processors: [{:ot_batch_processor, %{scheduled_delay_ms: 1}}]
