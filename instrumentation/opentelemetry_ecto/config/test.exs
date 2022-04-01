import Config

config :opentelemetry_ecto,
  ecto_repos: [
    OpentelemetryEcto.MariaDBTestRepo,
    OpentelemetryEcto.TestRepo
  ]

config :opentelemetry_ecto, OpentelemetryEcto.TestRepo,
  hostname: "localhost",
  username: "postgres",
  password: "postgres",
  database: "opentelemetry_ecto_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  telemetry_prefix: [:opentelemetry_ecto, :test_repo]

config :opentelemetry_ecto, OpentelemetryEcto.MariaDBTestRepo,
  hostname: "localhost",
  username: "mariadb",
  password: "mariadb",
  database: "opentelemetry_ecto_test",
  pool: Ecto.Adapters.SQL.Sandbox,
  telemetry_prefix: [:opentelemetry_ecto, :mariadb_test_repo]

config :opentelemetry,
  processors: [{:otel_batch_processor, %{scheduled_delay_ms: 1}}]
