import Config

if config_env() == :test do
  config :opentelemetry,
    processors: [{:otel_simple_processor, %{}}]
end
