OpentelemetryDataloader.TestRepo.start_link()

ExUnit.start(capture_log: true)

Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryDataloader.TestRepo, {:shared, self()})
