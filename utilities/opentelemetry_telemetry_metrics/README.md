# OtelTelemetryMetrics

A utility library for creating OpenTelemetry metrics from metric definitions
done with
[Telemetry.Metrics](https://github.com/beam-telemetry/telemetry_metrics) and
their corresponding telemetry events.

## Installation

```elixir
def deps do
  [
    {:opentelemetry_telemetry_metrics, "~> 0.1.0"}
  ]
end
```

## Usage

`OtelTelemetryMetrics.start_link/1` creates OpenTelemetry Instruments for
`Telemetry.Metric` metrics and records to them when their corresponding events
are triggered.

``` elixir
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

{:ok, _} = OtelTelemetryMetrics.start_link([metrics: metrics])
```

Then either in your Application code or a dependency execute `telemetry` events
containing the measurements. For example, an event that will result in the
metrics `vm.memory.total` and `vm.memory.binary` being recorded to:

```elixir
:telemetry.execute([:vm, :memory], %{binary: 100, total: 200}, %{})
```

OpenTelemetry does not support a `summary` type metric, the `summary`
`http.request.response_time` is recorded as a single bucket histogram.

In `Telemetry.Metrics` the `counter` type refers to counting the number of times
an event is triggered, this is represented as a `sum` in OpenTelemetry and when
recording the value is sent as a `1` every time.

Metrics of type `last_value` are ignored because `last_value` is not yet an
aggregation supported on synchronous instruments in Erlang/Elixir OpenTelemetry.
When it is added to the SDK this library will be updated to no longer ignore
metrics of this type.

## Dealing with InstrumentationScope

In OpenTelemetry [Instrumentation
Scope](https://opentelemetry.io/docs/concepts/instrumentation-scope/) is used to
associate emitted telemetry with the code it comes from. In Elixir this is
usually an OTP Application. Since the Scope needs to be defined when creating
the OpenTelemetry Instrument we must include it when creating a
`Telemetry.Metric` as a `reporter_opt`.

