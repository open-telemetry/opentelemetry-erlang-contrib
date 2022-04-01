defmodule OpentelemetryEcto.MariaDBTestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.MyXQL
end
