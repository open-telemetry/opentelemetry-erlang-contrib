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

  @fields Record.extract(:histogram_datapoint, from_lib: "opentelemetry_experimental/include/otel_metrics.hrl")
  Record.defrecordp(:histogram_datapoint, @fields)

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
        unit: {:byte, :megabyte},
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

    :telemetry.execute([:http, :request], %{response_time: 1000}, %{foo: :bar})

    # this one gets ignored
    :telemetry.execute([:http, :request], %{response_time: 2000}, %{boom: :pow,
                                                                    foo: :bar})

    :telemetry.execute([:db, :query], %{duration: 112}, %{table: "users", operation: "select"})
    :telemetry.execute([:db, :query], %{duration: 201}, %{table: "sessions", operation: "insert"})

    :telemetry.execute([:phoenix, :endpoint, :stop], %{duration: 100}, %{})

    :otel_meter_server.force_flush()

    # last_value type metrics are ignored at this time
    refute_receive {:metric, metric( name: :'vm.memory.binary')}

    assert_receive {:metric,
                    metric(
                      name: :'phoenix.endpoint.stop.duration',
                      data: histogram(datapoints: [histogram_datapoint(
                                                      attributes: %{},
                                                      count: 1,
                                                      sum: 100,
                                                      min: 100,
                                                      max: 100,
                                                      bucket_counts: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0])
                                                  ]
                      )
                    )}

    receive do
      {:metric,
       metric(
         name: :'db.query.duration',
         data: sum(datapoints: datapoints))} ->
        assert [datapoint(
                   attributes: %{table: "sessions", operation: "insert"},
                   value: 1),
                datapoint(
                  attributes: %{table: "users", operation: "select"},
                  value: 1)] = Enum.sort(datapoints)
    after
      1000 ->
        flunk(:timeout_query_duration)
    end


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
                      unit: :megabyte,
                      data: sum(datapoints: [datapoint(value: 4.0e-6)])
                    )}

    # response_time is a summary, which gets represented as a single bucket histogram
    # since the second event had metadata boom: pow it is ignored and count is just 1
    assert_receive {:metric,
                    metric(
                      name: :'http.request.response_time',
                      data: histogram(datapoints: [histogram_datapoint(
                                                      attributes: %{bar: :baz},
                                                      count: 1,
                                                      sum: 1000,
                                                      min: 1000,
                                                      max: 1000,
                                                      bucket_counts: [1])
                                                  ]
                      )
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
