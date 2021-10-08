defmodule OpentelemetryEcto.TestRepo.Migrations.SetupTables do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string
    end

    create table(:posts) do
      add :body, :text
      add :user_id, references(:users)
    end
  end
end
