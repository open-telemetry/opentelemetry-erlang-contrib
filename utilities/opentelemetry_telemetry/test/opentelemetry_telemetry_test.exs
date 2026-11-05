defmodule OpentelemetryTelemetryTest do
  use ExUnit.Case

  @tracer_id :opentelemetry_telemetry_test

  setup do
    {:ok, _} = Application.ensure_all_started(:opentelemetry)
    :ok
  end

  test "set_current_telemetry_span/2 returns undefined once the tracer stack is exhausted" do
    meta = %{}

    _ctx = :otel_telemetry.start_telemetry_span(@tracer_id, "exhausted", meta, %{})
    assert :ok = :otel_telemetry.end_telemetry_span(@tracer_id, meta)

    assert :undefined = :otel_telemetry.set_current_telemetry_span(@tracer_id, meta)
  end
end
