defmodule OpentelemetryCommanded.DummyApp.Handler do
  @behaviour Commanded.Commands.Handler

  alias OpentelemetryCommanded.DummyApp.Aggregate
  alias OpentelemetryCommanded.DummyApp.Commands, as: C
  alias OpentelemetryCommanded.DummyApp.Events, as: E

  def handle(%Aggregate{}, %C.Ok{id: id}), do: %E.OkEvent{id: id}
  def handle(%Aggregate{}, %C.Error{message: message}), do: {:error, message}
  def handle(%Aggregate{}, %C.RaiseException{message: "some error"}), do: raise("some error")
  def handle(%Aggregate{}, %C.DoEvent{event: event}), do: event
  def handle(%Aggregate{}, %C.ProcessManagerCommand{}), do: nil
end
