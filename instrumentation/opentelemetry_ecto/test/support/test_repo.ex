defmodule OpentelemetryEcto.TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_ecto,
    adapter: Ecto.Adapters.Postgres

  @replicas [
    OpentelemetryEcto.TestRepo.Replica1
  ]

  def replica do
    Enum.random(@replicas)
  end

  for repo <- @replicas do
    defmodule repo do
      use Ecto.Repo,
        otp_app: :opentelemetry_ecto,
        adapter: Ecto.Adapters.Postgres,
        read_only: true
    end
  end
end
