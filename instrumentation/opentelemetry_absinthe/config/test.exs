import Config

config :opentelemetry,
  traces_exporter: :none,
  processors: [{:otel_simple_processor, %{}}]
