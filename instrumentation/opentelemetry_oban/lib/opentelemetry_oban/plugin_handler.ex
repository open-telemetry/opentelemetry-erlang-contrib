defmodule OpentelemetryOban.PluginHandler do
  alias OpenTelemetry.Span

  @tracer_id __MODULE__

  @plugin_start [:oban, :plugin, :start]
  @plugin_stop [:oban, :plugin, :stop]
  @plugin_exception [:oban, :plugin, :exception]

  def attach do
    :telemetry.attach_many(
      [__MODULE__, :plugin],
      [
        @plugin_start,
        @plugin_stop,
        @plugin_exception
      ],
      &__MODULE__.handle_event/4,
      _config = %{}
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@plugin_start, _measurements, %{plugin: plugin} = metadata, _config) do
    span_name = "#{inspect(plugin)} process"

    attributes = %{
      "messaging.oban.plugin": inspect(plugin)
    }

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, %{
      attributes: attributes
    })
  end

  def handle_event(@plugin_stop, _measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    Span.set_attributes(ctx, plugin_work_attributes(metadata))
    maybe_record_scaler_error(ctx, metadata)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_event(@plugin_exception, _measurements, metadata, _config) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    exception = Exception.normalize(metadata.kind, metadata.reason, metadata.stacktrace)
    Span.record_exception(ctx, exception, metadata.stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, format_error(exception)))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp plugin_work_attributes(%{plugin: Oban.Stager} = metadata) do
    %{
      "messaging.oban.staged_count": metadata.staged_count
    }
  end

  defp plugin_work_attributes(%{plugin: Oban.Plugins.Gossip} = metadata) do
    %{
      "messaging.oban.gossip_count": metadata.gossip_count
    }
  end

  defp plugin_work_attributes(%{plugin: Oban.Plugins.Reindexer}), do: %{}

  defp plugin_work_attributes(%{plugin: plugin, jobs: jobs})
       when plugin in [Oban.Plugins.Cron, Oban.Pro.Plugins.DynamicCron] do
    %{
      "messaging.oban.jobs_count": length(jobs)
    }
  end

  defp plugin_work_attributes(%{plugin: plugin} = metadata)
       when plugin in [Oban.Plugins.Lifeline, Oban.Pro.Plugins.DynamicLifeline] do
    %{
      "messaging.oban.rescued_jobs_count": length(metadata.rescued_jobs),
      "messaging.oban.discarded_jobs_count": length(metadata.discarded_jobs)
    }
  end

  defp plugin_work_attributes(%{plugin: plugin} = metadata)
       when plugin in [Oban.Plugins.Pruner, Oban.Pro.Plugins.DynamicPruner] do
    pruned_count_by_state =
      Enum.reduce(metadata.pruned_jobs, %{}, fn %{state: state}, count ->
        Map.update(count, state, 1, &(&1 + 1))
      end)

    %{
      "messaging.oban.pruned_jobs_count": length(metadata.pruned_jobs),
      "messaging.oban.pruned_completed_jobs_count": Map.get(pruned_count_by_state, :completed, 0),
      "messaging.oban.pruned_cancelled_jobs_count": Map.get(pruned_count_by_state, :cancelled, 0),
      "messaging.oban.pruned_discarded_jobs_count": Map.get(pruned_count_by_state, :discarded, 0)
    }
  end

  defp plugin_work_attributes(%{plugin: Oban.Pro.Plugins.DynamicPrioritizer} = metadata) do
    %{
      "messaging.oban.reprioritized_jobs_count": metadata.reprioritized_count
    }
  end

  defp plugin_work_attributes(%{plugin: Oban.Pro.Plugins.DynamicQueues}), do: %{}

  defp plugin_work_attributes(%{plugin: Oban.Pro.Plugins.DynamicScaler} = metadata) do
    {module, _opts} = metadata.cloud

    %{
      "messaging.oban.scaler.cloud": inspect(module),
      "messaging.oban.scaler.last_rate": metadata.scaler.last_rate,
      "messaging.oban.scaler.last_scaled_to": metadata.scaler.last_scaled_to
    }
  end

  defp plugin_work_attributes(_metadata), do: %{}

  defp maybe_record_scaler_error(ctx, %{error: error}) do
    Span.set_status(ctx, OpenTelemetry.status(:error, format_error(error)))
  end

  defp maybe_record_scaler_error(_ctx, _metadata), do: :ok

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(error), do: inspect(error)
end
