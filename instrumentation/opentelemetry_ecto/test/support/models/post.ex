defmodule OpentelemetryEcto.TestModels.Post do
  use Ecto.Schema

  schema "posts" do
    field(:body, :string)
    belongs_to(:user, OpentelemetryEcto.TestModels.User)
  end
end
