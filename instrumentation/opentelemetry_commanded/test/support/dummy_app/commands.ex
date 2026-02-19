defmodule OpentelemetryCommanded.DummyApp.Commands do
  @moduledoc false

  defmodule Ok do
    defstruct [:id]
  end

  defmodule Error do
    defstruct [:id, :message]
  end

  defmodule RaiseException do
    defstruct [:id, :message]
  end

  defmodule DoEvent do
    defstruct [:id, :event]
  end

  defmodule DispatchError do
    defstruct [:id, :message]
  end

  defmodule ProcessManagerCommand do
    defstruct [:id]
  end
end
