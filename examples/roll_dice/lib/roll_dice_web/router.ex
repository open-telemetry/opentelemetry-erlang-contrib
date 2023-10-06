defmodule RollDiceWeb.Router do
  use RollDiceWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", RollDiceWeb do
    pipe_through :api

    get "/rolldice", DiceController, :roll
  end
end
