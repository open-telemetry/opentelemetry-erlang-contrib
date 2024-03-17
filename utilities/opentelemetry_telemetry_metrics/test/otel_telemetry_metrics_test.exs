defmodule OtelTelemetryMetricsTest do
  use ExUnit.Case
  doctest OtelTelemetryMetrics

  alias Telemetry.Metrics, as: Metrics

  require Record
  @fields Record.extract(:metric, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:metric, @fields)

  @fields Record.extract(:datapoint,
            from_lib: "opentelemetry_experimental/include/otel_metrics.hrl"
          )
  Record.defrecordp(:datapoint, @fields)

  @fields Record.extract(:sum, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:sum, @fields)

  @fields Record.extract(:histogram,
            from_lib: "opentelemetry_experimental/include/otel_metrics.hrl"
          )
  Record.defrecordp(:histogram, @fields)

  def metadata_measurement(_measurements, metadata) do
    map_size(metadata)
  end

  def measurement(%{duration: duration} = _measurement) do
    duration
  end

  setup do
    metrics = [
      Metrics.last_value("vm.memory.binary", unit: :byte),
      Metrics.counter("vm.memory.total"),
      Metrics.counter("db.query.duration", tags: [:table, :operation]),
      Metrics.summary("http.request.response_time",
        tag_values: fn
          %{foo: :bar} -> %{bar: :baz}
        end,
        tags: [:bar],
        drop: fn metadata ->
          metadata[:boom] == :pow
        end
      ),
      Metrics.sum("telemetry.event_size.metadata",
        measurement: &__MODULE__.metadata_measurement/2
      ),
      Metrics.distribution("phoenix.endpoint.stop.duration",
        measurement: &__MODULE__.measurement/1
      )
    ]

    Application.load(:opentelemetry_experimental)
    Application.load(:opentelemetry)

    Application.put_env(:opentelemetry, :processors, [
      {:otel_simple_processor, %{exporter: :none}}
        ])

    Application.put_env(:opentelemetry_experimental, :readers, [
      %{
        module: :otel_metric_reader,
        config: %{
          exporter: {:otel_metric_exporter_pid, {:metric, self()}}
        }
      }
    ])

    {:ok, _} = Application.ensure_all_started(:opentelemetry_experimental)

    {:ok, _} = Application.ensure_all_started(:telemetry)
    {:ok, _} = OtelTelemetryMetrics.start_link([metrics: metrics])

    on_exit(fn ->
      Application.stop(:opentelemetry_experimental)
      Application.unload(:opentelemetry_experimental)
      Application.stop(:opentelemetry)
      Application.unload(:opentelemetry)
    end)

    :ok
  end

  test "event with two measurements" do
    :telemetry.execute([:vm, :memory], %{binary: 100, total: 200}, %{})
    :telemetry.execute([:vm, :memory], %{total: 300}, %{})
    :telemetry.execute([:telemetry, :event_size], %{}, %{key1: :value1,
                                                         key2: :value2})

    :telemetry.execute([:telemetry, :event_size], %{}, %{key1: :value1,
                                                         key2: :value2})

    # :telemetry.execute([:db, :query], %{duration: 112}, %{table: "users", operation: "select"})
    # :telemetry.execute([:db, :query], %{duration: 201}, %{table: "sessions", operation: "insert"})

    :otel_meter_server.force_flush()

    # Metric for vm_memory_total is their form of Counter, a metric that increments
    # by 1 on each recording. So should be only 1 here
    assert_receive {:metric,
                    metric(
                      name: :'vm.memory.total',
                      data: sum(datapoints: [datapoint(value: 2)])
                    )}
    assert_receive {:metric,
                    metric(
                      name: :'telemetry.event_size.metadata',
                      data: sum(datapoints: [datapoint(value: 4)])
                    )}

    :telemetry.execute([:vm, :memory], %{binary: 100, total: 500}, %{})

    :otel_meter_server.force_flush()

    # counter temporality is delta so should reset to 0
    assert_receive {:metric,
                    metric(
                      name: :'vm.memory.total',
                      data: sum(datapoints: [datapoint(value: 1)])
                    )}
  end
end
