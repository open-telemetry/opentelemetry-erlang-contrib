defmodule RollDice.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    :opentelemetry_cowboy.setup()
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    children = [
      # Start the Telemetry supervisor
      RollDiceWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: RollDice.PubSub},
      # Start the Endpoint (http/https)
      RollDiceWeb.Endpoint
      # Start a worker by calling: RollDice.Worker.start_link(arg)
      # {RollDice.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: RollDice.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    RollDiceWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
