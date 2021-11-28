defmodule BasicElixirTest do
  use ExUnit.Case
  doctest BasicElixir

  test "greets the world" do
    assert BasicElixir.hello() == :world
  end
end
