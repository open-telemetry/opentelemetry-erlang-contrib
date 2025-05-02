defmodule TestJob do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    :ok
  end
end
