defmodule OpentelemetryOban.PluginHandler do
  @moduledoc false

  alias OpenTelemetry.Tracer
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemConv.ErrorAttributes

  @tracer_id __MODULE__

  def attach() do
    attach_plugin_start_handler()
    attach_plugin_stop_handler()
    attach_plugin_exception_handler()
  end

  defp attach_plugin_start_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_start",
      [:oban, :plugin, :start],
      &__MODULE__.handle_plugin_start/4,
      []
    )
  end

  defp attach_plugin_stop_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_stop",
      [:oban, :plugin, :stop],
      &__MODULE__.handle_plugin_stop/4,
      []
    )
  end

  defp attach_plugin_exception_handler() do
    :telemetry.attach(
      "#{__MODULE__}.plugin_exception",
      [:oban, :plugin, :exception],
      &__MODULE__.handle_plugin_exception/4,
      []
    )
  end

  def handle_plugin_start(_event, _measurements, %{plugin: plugin} = metadata, _config) do
    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      "#{inspect(plugin)} process",
      metadata,
      %{attributes: %{"oban.plugin": inspect(plugin)}}
    )
  end

  def handle_plugin_stop(_event, _measurements, metadata, _config) do
    Tracer.set_attributes(end_span_plugin_attrs(metadata))
    maybe_record_stop_error(metadata)
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_plugin_exception(
        _event,
        _measurements,
        %{kind: kind, reason: reason, stacktrace: stacktrace} = metadata,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    error = record_exception(ctx, kind, reason, stacktrace)

    Span.set_status(
      ctx,
      OpenTelemetry.status(:error, Exception.format_banner(kind, reason, stacktrace))
    )

    set_error_type(error)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  # `Exception.normalize/3` turns Erlang-style reasons (`:badarg`, `:undef`, ...) into Elixir
  # exception structs, so `Span.record_exception/4` can record `exception.message`. Reasons that
  # cannot be normalized (`:throw` and `:exit` kinds) fall back to the Erlang API, which accepts
  # any term but records no message.
  defp record_exception(ctx, kind, reason, stacktrace) do
    case Exception.normalize(kind, reason, stacktrace) do
      %{__exception__: true} = exception ->
        Span.record_exception(ctx, exception, stacktrace, [])
        exception

      not_an_exception ->
        :otel_span.record_exception(ctx, kind, reason, stacktrace, [])
        not_an_exception
    end
  end

  # Plugins report failure through the `{:error, meta}` return value of `:telemetry.span/3`, which
  # emits a `:stop` event carrying `:error` instead of an `:exception` event.
  #
  # `Oban.Plugins.Reindexer` has no `else` branch for the non-leader case, so every follower node
  # reports `nil` on each scheduled run. That is a no-op rather than a failure, and it carries no
  # diagnostic value, so it is not recorded as an error.
  defp maybe_record_stop_error(%{error: nil}), do: :ok

  defp maybe_record_stop_error(%{error: error}) do
    reason = unwrap_error(error)

    Tracer.set_status(OpenTelemetry.status(:error, format_error(reason)))
    set_error_type(reason)
  end

  defp maybe_record_stop_error(_metadata), do: :ok

  # Most plugins put their whole `{:error, reason}` return value under `:error`; `Oban.Stager`
  # destructures it and puts the bare reason.
  defp unwrap_error({:error, reason}), do: reason
  defp unwrap_error(error), do: error

  defp format_error(error) when is_exception(error), do: Exception.message(error)
  defp format_error(error), do: inspect(error)

  defp set_error_type(%struct_name{} = error) when is_exception(error),
    do: Tracer.set_attribute(ErrorAttributes.error_type(), inspect(struct_name))

  defp set_error_type(_error), do: :ok

  defp end_span_plugin_attrs(%{plugin: Oban.Plugins.Cron} = metadata) do
    %{"oban.plugins.cron.jobs_count": length(metadata[:jobs] || [])}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Plugins.Gossip} = metadata) do
    %{"oban.plugins.gossip.gossip_count": metadata[:gossip_count]}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Plugins.Lifeline} = metadata) do
    %{
      "oban.plugins.lifeline.discarded_count": length(metadata[:discarded_jobs] || []),
      "oban.plugins.lifeline.rescued_count": length(metadata[:rescued_jobs] || [])
    }
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Plugins.Pruner} = metadata) do
    %{"oban.plugins.pruner.pruned_count": metadata[:pruned_count]}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Pro.Plugins.DynamicCron} = metadata) do
    %{"oban.pro.plugins.dynamic_cron.jobs_count": length(metadata[:jobs] || [])}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Pro.Plugins.DynamicLifeline} = metadata) do
    %{
      "oban.pro.plugins.dynamic_lifeline.discarded_count":
        length(metadata[:discarded_jobs] || []),
      "oban.pro.plugins.dynamic_lifeline.rescued_count": length(metadata[:rescued_jobs] || [])
    }
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Pro.Plugins.DynamicPrioritizer} = metadata) do
    %{"oban.pro.plugins.dynamic_prioritizer.reprioritized_count": metadata[:reprioritized_count]}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Pro.Plugins.DynamicPruner} = metadata) do
    %{"oban.pro.plugins.dynamic_pruner.pruned_count": metadata[:pruned_count]}
  end

  defp end_span_plugin_attrs(%{plugin: Oban.Pro.Plugins.DynamicScaler} = metadata) do
    %{
      "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_to": metadata[:scaler][:last_scaled_to],
      "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_at":
        DateTime.to_iso8601(metadata[:scaler][:last_scaled_at])
    }
  end

  defp end_span_plugin_attrs(_) do
    %{}
  end
end
