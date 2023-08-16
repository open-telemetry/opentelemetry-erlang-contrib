import Config

config :opentelemetry,
  processors: [:simple],
  traces_exporter: :none
