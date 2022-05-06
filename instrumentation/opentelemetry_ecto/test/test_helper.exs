OpentelemetryEcto.TestRepo.start_link()

ExUnit.start(capture_log: true)

Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.TestRepo, {:shared, self()})
