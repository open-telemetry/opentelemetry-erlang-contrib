defmodule NnnnnWeb.MyTestLive do
  use Phoenix.LiveView, log: false

  def render(assigns), do: ~H""
end

defmodule NnnnnWeb.Router do
  use Phoenix.Router, helpers: false

  import Phoenix.LiveView.Router

  live("/live", NnnnnWeb.MyTestLive, :index)
  live("/resources/:resource_id", NnnnnWeb.MyTestLive, :show)
end
