import Config

postgres_repos = %{
  OpentelemetryEcto.TestRepo => %{port: 5432, hostname: "localhost"},
  OpentelemetryEcto.TestRepo.Replica1 => %{port: 5433, hostname: "127.0.0.1"}
}

for {repo, %{hostname: hostname, port: port}} <- postgres_repos do
  config :opentelemetry_ecto, repo,
    username: "postgres",
    password: "postgres",
    database: "opentelemetry_ecto_test",
    hostname: hostname,
    port: port,
    pool: Ecto.Adapters.SQL.Sandbox
end

config :opentelemetry_ecto, OpentelemetryEcto.MyXQLTestRepo,
  username: "root",
  password: "mysql",
  database: "opentelemetry_ecto_test",
  hostname: "localhost",
  port: 3306,
  pool: Ecto.Adapters.SQL.Sandbox,
  priv: "priv/test_repo"

config :opentelemetry_ecto, OpentelemetryEcto.TdsTestRepo,
  username: "sa",
  password: "MSSQLpass1!",
  database: "opentelemetry_ecto_test",
  hostname: "localhost",
  port: 1433,
  pool: Ecto.Adapters.SQL.Sandbox,
  priv: "priv/test_repo"

config :opentelemetry_ecto, OpentelemetryEcto.Sqlite3TestRepo,
  # username: "sa",
  # password: "MSSQLpass1!",
  database: "opentelemetry_ecto_test.db",

  # hostname: "localhost",
  # port: 1433,
  pool: Ecto.Adapters.SQL.Sandbox,
  priv: "priv/test_repo"

config :opentelemetry_ecto,
  ecto_repos: [
    OpentelemetryEcto.TestRepo,
    OpentelemetryEcto.TestRepo.Replica1,
    OpentelemetryEcto.MyXQLTestRepo,
    OpentelemetryEcto.TdsTestRepo,
    OpentelemetryEcto.Sqlite3TestRepo
  ]

config :opentelemetry,
  processors: [{:otel_batch_processor, %{scheduled_delay_ms: 1}}]
