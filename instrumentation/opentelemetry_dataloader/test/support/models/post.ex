defmodule OpentelemetryDataloader.TestModels.Post do
  use Ecto.Schema

  schema "posts" do
    field(:body, :string)
  end
end
