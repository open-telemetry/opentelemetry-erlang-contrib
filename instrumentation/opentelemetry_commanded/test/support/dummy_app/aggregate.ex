defmodule OpentelemetryCommanded.DummyApp.Aggregate do
  @moduledoc false

  @derive Jason.Encoder
  defstruct [:id, calls: 0]

  alias OpentelemetryCommanded.DummyApp.Aggregate

  def apply(%Aggregate{} = state, _event), do: %Aggregate{state | calls: state.calls + 1}
end
