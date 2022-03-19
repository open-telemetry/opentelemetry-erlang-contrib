ExUnit.start()

TestRepo.start_link(
  database: "opentelemetry_oban_test",
  hostname: "localhost",
  username: "postgres",
  password: "postgres",
  pool: Ecto.Adapters.SQL.Sandbox
)

Ecto.Adapters.SQL.Sandbox.mode(TestRepo, {:shared, self()})

defmodule PrepareOban do
  use Ecto.Migration
  def up, do: Oban.Migrations.up()
end

Ecto.Migrator.run(TestRepo, [{0, PrepareOban}], :up, all: true)
TestRepo.query("TRUNCATE oban_jobs", [])

Oban.start_link(
  repo: TestRepo,
  plugins: [Oban.Plugins.Pruner],
  queues: [default: 10, events: 50]
)

defmodule TestJob do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    :ok
  end
end

defmodule TestJobWithInnerSpan do
  use Oban.Worker, queue: :events, max_attempts: 1
  require OpenTelemetry.Tracer

  @impl Oban.Worker
  def perform(_job) do
    OpenTelemetry.Tracer.with_span "span inside the job" do
      :ok
    end
  end
end

defmodule TestJobThatReturnsError do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    {:error, :something}
  end
end

defmodule TestJobThatThrowsException do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    raise %UndefinedFunctionError{
      message: "function Some.error/0 is undefined (module Some is not available)"
    }
  end
end

defmodule TestHelpers do
  def remove_oban_handlers() do
    Enum.each(:telemetry.list_handlers([:oban]), fn handler ->
      :telemetry.detach(handler[:id])
    end)
  end
end
