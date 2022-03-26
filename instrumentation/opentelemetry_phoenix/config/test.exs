import Config

config :opentelemetry,
  processors: [{:otel_simple_processor, %{}}]
