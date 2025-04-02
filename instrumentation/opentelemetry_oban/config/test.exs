import Config

config :opentelemetry_oban,
  ecto_repos: [TestRepo]

config :opentelemetry_oban, TestRepo,
  hostname: "localhost",
  username: "postgres",
  password: "postgres",
  database: "opentelemetry_oban_test",
  pool: Ecto.Adapters.SQL.Sandbox
