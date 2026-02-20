defmodule OpentelemetryCommanded.DummyApp.EventHandler do
  use Commanded.Event.Handler,
    application: OpentelemetryCommanded.DummyApp.App,
    name: "EventHandler",
    start_from: :current,
    consistency: :strong

  alias OpentelemetryCommanded.DummyApp.Events, as: E

  defstruct [:id]

  def handle(%E.OkEvent{}, _metadata), do: :ok
  def handle(%E.ErrorInEventHandlerEvent{} = event, _metadata), do: {:error, event.message}
  def handle(%E.ExceptionInEventHandlerEvent{} = event, _metadata), do: raise(event.message)

  def error(_error, _event, _ctx), do: :skip
end
