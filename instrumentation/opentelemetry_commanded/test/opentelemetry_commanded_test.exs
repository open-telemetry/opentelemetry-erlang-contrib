defmodule OpentelemetryCommandedTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryCommanded

  test "sets it up!" do
    running? =
      case OpentelemetryCommanded.setup() do
        :ok -> true
        {:error, :already_exists} -> true
      end

    assert running?
  end
end
