defmodule OpentelemetryOban.TestRepo.Migrations.SetupTables do
  use Ecto.Migration

  def up, do: Oban.Migrations.up()
end
