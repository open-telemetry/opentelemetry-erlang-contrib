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
