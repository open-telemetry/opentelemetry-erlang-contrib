defmodule OpentelemetryCommanded do
  @moduledoc File.read!("./README.md") |> String.split("\n") |> Enum.drop(2) |> Enum.join("\n")

  def setup do
    OpentelemetryCommanded.Application.setup()
    OpentelemetryCommanded.Aggregate.setup()
    OpentelemetryCommanded.EventHandler.setup()
    OpentelemetryCommanded.EventStore.setup()
    OpentelemetryCommanded.ProcessManager.setup()
  end
end
