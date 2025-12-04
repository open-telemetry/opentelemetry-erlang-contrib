defmodule OpentelemetryCommanded.DummyApp.ProcessManager do
  use Commanded.ProcessManagers.ProcessManager,
    application: OpentelemetryCommanded.DummyApp.App,
    name: "ProcessManager"

  @derive Jason.Encoder
  defstruct [:id]

  alias OpentelemetryCommanded.DummyApp.Commands, as: C
  alias OpentelemetryCommanded.DummyApp.Events, as: E

  def interested?(%mod{} = event)
      when mod in [E.OkEvent, E.ErrorInProcessManagerEvent, E.ExceptionInProcessManagerEvent] do
    {:start, event.id}
  end

  def interested?(_event) do
    false
  end

  def handle(_pm, %E.OkEvent{id: id}) do
    %C.ProcessManagerCommand{id: id}
  end

  def handle(_pm, %E.ErrorInProcessManagerEvent{} = event) do
    {:error, event.message}
  end

  def handle(_pm, %E.ExceptionInProcessManagerEvent{} = event) do
    raise event.message
  end

  def error(_error, _command_or_event, _failure_context) do
    :skip
  end
end
