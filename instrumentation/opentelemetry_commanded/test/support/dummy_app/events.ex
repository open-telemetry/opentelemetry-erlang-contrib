defmodule OpentelemetryCommanded.DummyApp.Events do
  @moduledoc false

  defmodule OkEvent do
    @derive Jason.Encoder
    defstruct [:id]
  end

  defmodule ErrorInEventHandlerEvent do
    @derive Jason.Encoder
    defstruct [:id, :message]
  end

  defmodule ExceptionInEventHandlerEvent do
    @derive Jason.Encoder
    defstruct [:id, :message]
  end

  defmodule ErrorInProcessManagerEvent do
    @derive Jason.Encoder
    defstruct [:id, :message]
  end

  defmodule ExceptionInProcessManagerEvent do
    @derive Jason.Encoder
    defstruct [:id, :message]
  end
end
