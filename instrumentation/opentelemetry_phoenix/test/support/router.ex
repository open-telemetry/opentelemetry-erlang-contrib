defmodule OpentelemetryPhoenix.TestSupport.MyTestLive do
  use Phoenix.LiveView, log: false

  def render(assigns), do: ~H""
end

defmodule OpentelemetryPhoenix.TestSupport.Router do
  use Phoenix.Router, helpers: false

  import Phoenix.LiveView.Router

  live("/live", OpentelemetryPhoenix.TestSupport.MyTestLive, :index)
  live("/resources/:resource_id", OpentelemetryPhoenix.TestSupport.MyTestLive, :show)
end
