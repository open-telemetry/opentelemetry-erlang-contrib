ExUnit.start(capture_log: true)

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
