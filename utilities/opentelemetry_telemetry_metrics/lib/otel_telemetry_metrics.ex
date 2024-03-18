defmodule OtelTelemetryMetrics do
  @moduledoc """
  `OtelTelemetryMetrics.start_link/1` creates OpenTelemetry Instruments for
  `Telemetry.Metric` metrics and records to them when their corresponding
  events are triggered.

      metrics = [
        last_value("vm.memory.binary", unit: :byte),
        counter("vm.memory.total"),
        counter("db.query.duration", tags: [:table, :operation]),
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

      {:ok, _} = OtelTelemetryMetrics.start_link([metrics: metrics])

  Then either in your Application code or a dependency execute `telemetry`
  events conataining the measurements. For example, an event that will result
  in the metrics `vm.memory.total` and `vm.memory.binary` being recorded to:

      :telemetry.execute([:vm, :memory], %{binary: 100, total: 200}, %{})

  OpenTelemetry does not support a `summary` type metric, the `summary`
  `http.request.response_time` is recorded as a single bucket histogram.

  In `Telemetry.Metrics` the `counter` type refers to counting the number of
  times an event is triggered, this is represented as a `sum` in OpenTelemetry
  and when recording the value is sent as a `1` every time.

  Metrics of type `last_value` are ignored because `last_value` is not yet an
  aggregation supported on synchronous instruments in Erlang/Elixir
  OpenTelemetry. When it is added to the SDK this library will be updated to
  no longer ignore metrics of this type.
  """

  require Logger
  use GenServer

  @doc """
  """
  def start_link(options) do
    GenServer.start_link(__MODULE__, options)
  end

  @impl true
  def init(options) do
    Process.flag(:trap_exit, true)

    meter = options[:meter] || :opentelemetry_experimental.get_meter()
    metrics = options[:metrics] || []

    handler_ids = create_instruments_and_attach(meter, metrics)

    {:ok, %{handler_ids: handler_ids}}
  end

  @impl true
  def terminate(_, %{handler_ids: handler_ids}) do
    detach(handler_ids)
    :ok
  end

  defp create_instruments_and_attach(meter, metrics) do
    metrics_by_event = Enum.group_by(metrics, & &1.event_name)

    for {event_name, metrics} <- metrics_by_event do
      instruments = create_instruments(meter, metrics)

      attach(meter, event_name, instruments)
    end
  end

  defp create_instruments(meter, metrics) do
    for metric <- metrics,
      instrument = create_instrument(metric, meter, %{unit: unit(metric.unit)}),
      instrument != nil, into: %{} do
        {metric, instrument}
    end
  end

  defp create_instrument(%Telemetry.Metrics.Counter{}=metric, meter, opts) do
    :otel_counter.create(meter, format_name(metric), opts)
  end

  # a summary is represented as an explicit histogram with a single bucket
  defp create_instrument(%Telemetry.Metrics.Summary{}=metric, meter, opts) do
    :otel_histogram.create(meter, format_name(metric), Map.put(opts, :advisory_params, %{explicit_bucket_boundaries: []}))
  end

  defp create_instrument(%Telemetry.Metrics.Distribution{}=metric, meter, opts) do
    :otel_histogram.create(meter, format_name(metric), opts)
  end

  defp create_instrument(%Telemetry.Metrics.Sum{}=metric, meter, opts) do
    :otel_counter.create(meter, format_name(metric), opts)
  end

  # waiting on
  defp create_instrument(%Telemetry.Metrics.LastValue{}=metric, _meter, _) do
    Logger.info("Ignoring metric #{inspect(metric.name)} because LastValue aggregation is not supported in this version of OpenTelemetry Elixir")
    nil
  end

  defp unit(:unit), do: 1
  defp unit(unit), do: unit

  defp format_name(metric) do
    metric.name
    |> Enum.join(".")
    |> String.to_atom
  end

  defp attach(meter, event_name, instruments) do
    handler_id = handler_id(event_name)

    :ok =
      :telemetry.attach(handler_id, event_name, &__MODULE__.handle_event/4,
        %{meter: meter,
          instruments: instruments})

    handler_id
  end

  defp detach(handler_ids) do
    Enum.each(handler_ids, fn id -> :telemetry.detach(id) end)
  end

  defp handler_id(event_name) do
    {__MODULE__, event_name, self()}
  end

  def handle_event(_event_name, measurements, metadata, %{meter: meter,
                                                          instruments: instruments}) do
    for {metric, instrument} <- instruments do
      if value = keep?(metric, metadata) && extract_measurement(metric, measurements, metadata) do
        ctx = OpenTelemetry.Ctx.get_current()
        tags = extract_tags(metric, metadata)
        :otel_meter.record(ctx, meter, instrument, value, tags)
      end
    end
  end


  defp keep?(%{keep: nil}, _metadata), do: true
  defp keep?(%{keep: keep}, metadata), do: keep.(metadata)

  defp extract_measurement(%Telemetry.Metrics.Counter{}, _measurements, _metadata) do
    1
  end

  defp extract_measurement(metric, measurements, metadata) do
    case metric.measurement do
      nil ->
        nil

      fun when is_function(fun, 1) ->
        fun.(measurements)

      fun when is_function(fun, 2) ->
        fun.(measurements, metadata)

      key ->
        measurements[key] || 1
    end
  end

  defp extract_tags(metric, metadata) do
    tag_values = metric.tag_values.(metadata)
    Map.take(tag_values, metric.tags)
  end
end
