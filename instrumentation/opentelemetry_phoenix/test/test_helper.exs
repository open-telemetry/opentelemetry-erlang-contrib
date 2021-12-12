ExUnit.start()
Code.put_compiler_option(:warnings_as_errors, true)

defmodule TestHelpers do
  def remove_phoenix_handlers() do
    Enum.each(:telemetry.list_handlers([:phoenix]), fn handler ->
      :telemetry.detach(handler[:id])
    end)
  end
end
