defmodule OpentelemetryEcto.TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.Postgres,
    telemetry_prefix: [:opentelemetry_ecto, :test_repo]
end
