defmodule TestJobThatThrowsException do
  use Oban.Worker, queue: :events, max_attempts: 1

  @impl Oban.Worker
  def perform(_job) do
    raise %UndefinedFunctionError{
      message: "function Some.error/0 is undefined (module Some is not available)"
    }
  end
end
