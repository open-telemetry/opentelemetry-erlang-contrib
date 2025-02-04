defmodule OpentelemetryAbsintheTest.Case do
  @moduledoc false

  use ExUnit.CaseTemplate

  setup do
    on_exit(&reset_state/0)
  end

  defp reset_state do
    Application.delete_env(:opentelemetry_absinthe, :trace_options)
    OpentelemetryAbsinthe.Instrumentation.teardown()
    :ok
  end
end
