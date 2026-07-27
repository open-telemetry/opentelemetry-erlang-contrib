defmodule ConfigurationTest do
  alias OpentelemetryHTTPoison.Configuration

  use OpentelemetryHTTPoison.Case

  test "it does not crash on `get` if it has not been setup" do
    assert Configuration.get(:infer_route)
    assert Configuration.get(:ot_attributes)
  end

  test "if :ot_attributes are not a list it raises an error" do
    assert_raise ArgumentError, fn -> Configuration.setup(ot_attributes: {:key, :value}) end
  end

  test "if :ot_attributes are a list it sets up successfully" do
    assert Configuration.setup(ot_attributes: [{:key, :value}]) == nil
  end

  test "if :infer_route is not a function it raises an error" do
    assert_raise ArgumentError, fn -> Configuration.setup(infer_route: :not_a_function) end
  end

  test "if :infer_route is a function of arity other than 1 it raises an error" do
    assert_raise ArgumentError, fn ->
      Configuration.setup(infer_route: fn x, y, z -> {x, y, z} end)
    end
  end

  test "if :infer_route is a function of arity 1 it sets up successfully" do
    assert Configuration.setup(infer_route: fn x -> x end) == nil
  end
end
