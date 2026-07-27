defmodule OpentelemetryDataloader.TestRepo.Migrations.SetupTables do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add(:body, :text)
    end
  end
end
