defmodule OpentelemetryEcto.TdsTestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.Tds
end
