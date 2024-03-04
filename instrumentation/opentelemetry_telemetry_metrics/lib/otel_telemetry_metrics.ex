defmodule OtelTelemetryMetrics do
  @moduledoc """
  Documentation for `OtelTelemetryMetrics`.
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
    meter = options[:meter] || :opentelemetry_experimental.get_meter()
    metrics = options[:metrics] || []

    handler_ids = create_instruments_and_attach(meter, metrics)

    {:ok, %{handler_ids: handler_ids}}
  end


  defp create_instruments_and_attach(meter, metrics) do
    metrics_by_event = Enum.group_by(metrics, & &1.event_name)

    for {event_name, metrics} <- metrics_by_event do
      instruments = create_instruments(meter, metrics)

      attach(meter, event_name, instruments)
    end
  end

  def create_instruments(meter, metrics) do
    for metric <- metrics,
      instrument = create_instrument(metric, meter),
      instrument != nil, into: %{} do
        {metric, instrument}
    end
  end

  def create_instrument(%Telemetry.Metrics.Counter{}=metric, meter) do
    :otel_counter.create(meter, format_name(metric), %{})
  end

  # a summary is represented as an explicit histogram with a single bucket
  def create_instrument(%Telemetry.Metrics.Summary{}=metric, meter) do
    :otel_histogram.create(meter, format_name(metric), %{explicit_bucket_boundaries: []})
  end

  def create_instrument(%Telemetry.Metrics.Distribution{}=metric, meter) do
    :otel_histogram.create(meter, format_name(metric), %{})
  end

  def create_instrument(%Telemetry.Metrics.Sum{}=metric, meter) do
    :otel_counter.create(meter, format_name(metric), %{})
  end

  # waiting on
  def create_instrument(%Telemetry.Metrics.LastValue{}=metric, _meter) do
    Logger.info("Ignoring metric #{inspect(metric.name)} because LastValue aggregation is not supported in this version of OpenTelemetry Elixir")
    nil
  end

  defp format_name(metric) do
    metric.name
    |> Enum.join("_")
    |> String.to_atom
  end

  def attach(meter, event_name, instruments) do
    handler_id = handler_id(event_name)

    :ok =
      :telemetry.attach(handler_id, event_name, &__MODULE__.handle_event/4,
        %{meter: meter,
          instruments: instruments})

      handler_id
  end

  def detach(handler_ids) do
    Enum.each(handler_ids, fn id -> :telemetry.detach(id) end)
  end

  def handler_id(event_name) do
    {__MODULE__, event_name, self()}
  end


  def handle_event(_event_name, measurements, metadata, %{meter: meter,
                                                          instruments: instruments}) do
    for {metric, instrument} <- instruments do
      if value = keep?(metric, metadata) && fetch_measurement(metric, measurements, metadata) do
          ctx = OpenTelemetry.Ctx.get_current()
          :otel_meter.record(ctx, meter, instrument, value, metadata)
      end
    end
  end


  defp keep?(%{keep: nil}, _metadata), do: true
  defp keep?(%{keep: keep}, metadata), do: keep.(metadata)

  defp fetch_measurement(%Telemetry.Metrics.Counter{}, _measurements, _metadata) do
    1
  end

  defp fetch_measurement(metric, measurements, metadata) do
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

end
