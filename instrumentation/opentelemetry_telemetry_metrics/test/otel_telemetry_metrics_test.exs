defmodule OtelTelemetryMetricsTest do
  use ExUnit.Case
  doctest OtelTelemetryMetrics

  import Telemetry.Metrics

  def metadata_measurement(_measurements, metadata) do
    map_size(metadata)
  end

  def measurement(%{duration: duration} = _measurement) do
    duration
  end

  setup do
    metrics = [
      last_value("vm.memory.binary", unit: :byte),
      counter("vm.memory.total"),
      summary("http.request.response_time",
        tag_values: fn
          %{foo: :bar} -> %{bar: :baz}
        end,
        tags: [:bar],
        drop: fn metadata ->
          metadata[:boom] == :pow
        end
      ),
      sum("telemetry.event_size.metadata",
        measurement: &__MODULE__.metadata_measurement/2
      ),
      distribution("phoenix.endpoint.stop.duration",
        measurement: &__MODULE__.measurement/1
      )
    ]

    opts = [metrics: metrics]
    {:ok, _} = OtelTelemetryMetrics.start_link(opts)
    :ok
  end

  test "greets the world" do
    :telemetry.execute([:vm, :memory], %{binary: 100, total: 200}, %{})
  end
end
