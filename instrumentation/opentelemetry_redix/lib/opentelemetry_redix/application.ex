defmodule OpentelemetryRedix.Application do
  @moduledoc false
  use Application

  alias OpentelemetryRedix.ConnectionTracker

  def start(_type, _args) do
    children = [
      {ConnectionTracker, []}
    ]

    opts = [strategy: :one_for_one, name: OpentelemetryRedix.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
