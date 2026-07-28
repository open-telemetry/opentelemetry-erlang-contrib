defmodule TestJobThatReturnsError do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    {:error, :something}
  end
end
