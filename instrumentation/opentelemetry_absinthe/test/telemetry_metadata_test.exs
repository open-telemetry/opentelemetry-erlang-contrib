defmodule OpentelemetryAbsinthe.TelemetryMetadataTest do
  use ExUnit.Case

  alias OpentelemetryAbsinthe.TelemetryMetadata

  test "should return an empty metadata when context is empty" do
    assert %{} == TelemetryMetadata.from_context(%{})
  end

  test "should return an empty metadata when context does not contain metadata" do
    assert %{} == TelemetryMetadata.from_context(%{foo: :bar})
  end

  test "should return same metadata that was stored" do
    assert %{user_agent: :test} ==
             %{}
             |> TelemetryMetadata.update_context(%{user_agent: :test})
             |> TelemetryMetadata.from_context()
  end
end
