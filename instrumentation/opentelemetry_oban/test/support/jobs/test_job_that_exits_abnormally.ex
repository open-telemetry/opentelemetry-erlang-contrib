defmodule TestJobThatExitsAbnormally do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    exit(:abnormal)
  end
end
