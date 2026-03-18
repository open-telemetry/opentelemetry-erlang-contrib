defmodule TestJobThatThrowsValue do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    throw("value")
  end
end
