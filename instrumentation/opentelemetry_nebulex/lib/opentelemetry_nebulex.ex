defmodule OpentelemetryNebulex do
  @moduledoc """
  OpentelemetryNebulex uses `telemetry` handlers to create `OpenTelemetry` spans
  from Nebulex command events.
  """

  @tracer_id __MODULE__

  @doc """
  Initializes and configures telemetry handlers for a given cache.

  Example:

      OpentelemetryNebulex.setup([:blog, :partitioned_cache])
  """
  def setup(event_prefix, opts \\ []) do
    :telemetry.attach(
      {__MODULE__, event_prefix, :command_start},
      event_prefix ++ [:command, :start],
      &__MODULE__.handle_command_start/4,
      opts
    )

    :telemetry.attach(
      {__MODULE__, event_prefix, :command_stop},
      event_prefix ++ [:command, :stop],
      &__MODULE__.handle_command_stop/4,
      opts
    )

    :telemetry.attach(
      {__MODULE__, event_prefix, :command_exception},
      event_prefix ++ [:command, :exception],
      &__MODULE__.handle_command_exception/4,
      opts
    )
  end

  @doc """
  Initializes and configures telemetry handlers for all caches.

  Use the `[:nebulex, :cache, :init]` event to automatically discover caches, and attach
  the handlers dynamically. It only works for caches that start after this function is called.

  Example:

      OpentelemetryNebulex.setup_all()
  """
  def setup_all(opts \\ []) do
    :telemetry.attach(
      __MODULE__,
      [:nebulex, :cache, :init],
      &__MODULE__.handle_init/4,
      opts
    )
  end

  @doc false
  def handle_init(_event, _measurements, metadata, config) do
    setup(metadata[:opts][:telemetry_prefix], config)
  end

  @doc false
  def handle_command_start(_event, _measurements, metadata, _config) do
    span_name = "nebulex #{metadata.function_name}"

    attributes =
      %{
        "nebulex.cache": metadata.adapter_meta.cache
      }
      |> maybe_put(:"nebulex.backend", metadata.adapter_meta[:backend])
      |> maybe_put(:"nebulex.keyslot", metadata.adapter_meta[:keyslot])
      |> maybe_put(:"nebulex.model", metadata.adapter_meta[:model])

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, %{
      attributes: attributes
    })
  end

  @doc false
  def handle_command_stop(_event, _measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    if action = extract_action(metadata) do
      OpenTelemetry.Span.set_attribute(ctx, :"nebulex.action", action)
    end

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  @doc false
  def handle_command_exception(_event, _measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    OpenTelemetry.Span.record_exception(ctx, metadata.reason, metadata.stacktrace)
    OpenTelemetry.Tracer.set_status(OpenTelemetry.status(:error, format_error(metadata.reason)))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp maybe_put(attributes, _key, nil), do: attributes
  defp maybe_put(attributes, key, value), do: Map.put(attributes, key, value)

  defp extract_action(%{function_name: f, result: :"$expired"}) when f in [:get, :take], do: :miss
  defp extract_action(%{function_name: f, result: nil}) when f in [:get, :take], do: :miss
  defp extract_action(%{function_name: f, result: _}) when f in [:get, :take], do: :hit
  defp extract_action(_), do: nil

  defp format_error(exception) when is_exception(exception), do: Exception.message(exception)
  defp format_error(error), do: inspect(error)
end
