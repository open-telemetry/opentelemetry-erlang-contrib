import Config

config :opentelemetry_ecto,
  ecto_repos: [OpentelemetryEcto.TestRepo]

config :opentelemetry_ecto, OpentelemetryEcto.TestRepo,
  hostname: "localhost",
  username: "postgres",
  password: "postgres",
  port: 5432,
  database: "opentelemetry_ecto_test",
  pool: Ecto.Adapters.SQL.Sandbox

config :opentelemetry,
  processors: [{:otel_batch_processor, %{scheduled_delay_ms: 1}}]
