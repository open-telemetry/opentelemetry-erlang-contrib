defmodule OpentelemetryEcto.TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.Postgres
end
