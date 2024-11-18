defmodule TestHelpers do
  def remove_handlers do
    Enum.each(:telemetry.list_handlers([:broadway]), fn handler ->
      :telemetry.detach(handler[:id])
    end)
  end
end
