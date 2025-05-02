defmodule OpentelemetryEcto.Sqlite3TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.SQLite3
end
