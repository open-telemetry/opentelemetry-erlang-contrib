defmodule BlueprintArchitect do
  @moduledoc false

  alias Absinthe.Blueprint

  @spec blueprint(keyword()) :: Blueprint.t()
  def blueprint(overrides \\ []) do
    %{
      operations: [operation()]
    }
    |> Map.merge(Enum.into(overrides, %{}))
    |> then(&struct!(Blueprint, &1))
  end

  @spec operation(keyword()) :: Blueprint.Document.Operation.t()
  def operation(overrides \\ []) do
    %{
      name: "TestOperation",
      type: :query,
      current: true
    }
    |> Map.merge(Enum.into(overrides, %{}))
    |> then(&struct!(Blueprint.Document.Operation, &1))
  end

  @spec execution(keyword()) :: Blueprint.Execution.t()
  def execution(overrides \\ []) do
    %{}
    |> Map.merge(Enum.into(overrides, %{}))
    |> then(&struct!(Blueprint.Execution, &1))
  end
end
