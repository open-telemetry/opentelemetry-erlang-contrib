defmodule OpentelemetryEcto.TestModels.Comment do
  use Ecto.Schema

  schema "comments" do
    field(:body, :string)
    belongs_to(:user, OpentelemetryEcto.TestModels.User)
  end
end
