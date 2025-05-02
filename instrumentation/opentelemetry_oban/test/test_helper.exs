TestRepo.start_link()

ExUnit.start(capture_log: true)

Ecto.Adapters.SQL.Sandbox.mode(TestRepo, {:shared, self()})

Oban.start_link(
  repo: TestRepo,
  plugins: [Oban.Plugins.Pruner],
  notifier: Oban.Notifiers.PG,
  testing: :manual
)
