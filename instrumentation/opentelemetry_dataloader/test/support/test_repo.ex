defmodule OpentelemetryDataloader.TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_dataloader,
    adapter: Ecto.Adapters.Postgres
end
