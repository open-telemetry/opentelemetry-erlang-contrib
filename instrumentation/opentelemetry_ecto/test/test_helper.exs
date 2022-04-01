OpentelemetryEcto.MariaDBTestRepo.start_link()
OpentelemetryEcto.TestRepo.start_link()

ExUnit.start(capture_log: true)

Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.MariaDBTestRepo, {:shared, self()})
Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.TestRepo, {:shared, self()})
