defmodule Demo.AccountsFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Demo.Accounts` context.
  """

  @doc """
  Generate a user.
  """
  def user_fixture(attrs \\ %{}) do
    {:ok, user} =
      attrs
      |> Enum.into(%{
        name: "some name"
      })
      |> Demo.Accounts.create_user()

    user
  end
end
