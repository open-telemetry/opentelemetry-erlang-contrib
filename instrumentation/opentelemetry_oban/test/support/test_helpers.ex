defmodule TestHelpers do
  def remove_oban_handlers() do
    Enum.each(:telemetry.list_handlers([:oban]), fn handler ->
      :telemetry.detach(handler[:id])
    end)
  end
end
