defmodule DataCase do
  @moduledoc """
  This module defines the setup for tests requiring access to the data layer.

  You may define functions here to be used as helpers in your tests.

  Finally, if the test case interacts with the database, it cannot be async.
  For this reason, every test runs inside a transaction which is reset at the
  beginning of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      use Oban.Testing, repo: TestRepo

      import Ecto
      import DataCase
    end
  end

  setup _tags do
    start_link_supervised!({
      TestRepo,
      database: "tmp/test-#{:rand.uniform(10_000)}.db", journal_mode: :memory
    })

    Ecto.Migrator.run(TestRepo, [{0, PrepareOban}], :up, all: true)
    TestRepo.query("TRUNCATE oban_jobs", [])

    start_link_supervised!({
      Oban,
      repo: TestRepo,
      plugins: [Oban.Plugins.Pruner],
      notifier: Oban.Notifiers.PG,
      engine: Oban.Engines.Lite,
      testing: :manual
    })

    Path.wildcard("tmp/test-*.db")
    |> Enum.each(&File.rm!/1)

    :ok
  end
end
