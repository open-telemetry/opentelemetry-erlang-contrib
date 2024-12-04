defmodule OpentelemetryEcto.TestBrokenRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.Postgres

  def init(_context, config) do
    config = Keyword.drop(config, [:telemetry_prefix])
    {:ok, config}
  end
end
