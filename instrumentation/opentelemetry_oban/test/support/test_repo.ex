defmodule TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_oban,
    adapter: Ecto.Adapters.Postgres
end
