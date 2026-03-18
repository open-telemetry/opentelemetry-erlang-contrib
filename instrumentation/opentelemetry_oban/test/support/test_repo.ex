defmodule TestRepo do
  use Ecto.Repo,
    otp_app: :opentelemetry_oban,
    adapter: Ecto.Adapters.SQLite3
end

defmodule PrepareOban do
  use Ecto.Migration
  def up, do: Oban.Migrations.up()
end
