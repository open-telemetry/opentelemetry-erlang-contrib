defmodule OpentelemetryOban.JobHandler do
  @moduledoc false

  alias OpenTelemetry.Ctx
  alias OpenTelemetry.Span
  alias OpenTelemetry.Tracer
  alias OpenTelemetry.SemConv.Incubating.MessagingAttributes

  @tracer_id __MODULE__

  @options_schema [
    span_relationship: [
      type: {:in, [:child, :link, :none]},
      type_spec: quote(do: :child | :link | :none),
      default: :link,
      doc: """
      How spans relate to propagated parent context:
      * `:child` - Extract context and create parent-child relationships
      * `:link` - Extract context and create span links for loose coupling (default)
      * `:none` - Disable context propagation entirely
      """
    ]
  ]

  @nimble_options_schema NimbleOptions.new!(@options_schema)

  @doc false
  def options_schema do
    @options_schema
  end

  def attach(opts \\ []) do
    config =
      opts
      |> NimbleOptions.validate!(@nimble_options_schema)
      |> Enum.into(%{})

    attach_job_start_handler(config)
    attach_job_stop_handler(config)
    attach_job_exception_handler(config)
  end

  defp attach_job_start_handler(config) do
    :telemetry.attach(
      "#{__MODULE__}.job_start",
      [:oban, :job, :start],
      &__MODULE__.handle_job_start/4,
      config
    )
  end

  defp attach_job_stop_handler(config) do
    :telemetry.attach(
      "#{__MODULE__}.job_stop",
      [:oban, :job, :stop],
      &__MODULE__.handle_job_stop/4,
      config
    )
  end

  defp attach_job_exception_handler(config) do
    :telemetry.attach(
      "#{__MODULE__}.job_exception",
      [:oban, :job, :exception],
      &__MODULE__.handle_job_exception/4,
      config
    )
  end

  def handle_job_start(_event, _measurements, metadata, config) do
    %{
      job: %{
        id: id,
        queue: queue,
        worker: worker,
        priority: priority,
        inserted_at: inserted_at,
        scheduled_at: scheduled_at,
        attempt: attempt,
        max_attempts: max_attempts,
        meta: job_meta
      }
    } = metadata

    links = setup_context_propagation(job_meta, config.span_relationship)

    attributes = %{
      MessagingAttributes.messaging_system() => :oban,
      MessagingAttributes.messaging_operation_name() => "process",
      MessagingAttributes.messaging_message_id() => id,
      MessagingAttributes.messaging_client_id() => worker,
      MessagingAttributes.messaging_destination_name() => queue,
      MessagingAttributes.messaging_operation_type() =>
        MessagingAttributes.messaging_operation_type_values().process,
      :"oban.job.job_id" => id,
      :"oban.job.worker" => worker,
      :"oban.job.priority" => priority,
      :"oban.job.attempt" => attempt,
      :"oban.job.max_attempts" => max_attempts,
      :"oban.job.inserted_at" => DateTime.to_iso8601(inserted_at),
      :"oban.job.scheduled_at" => DateTime.to_iso8601(scheduled_at)
    }

    span_name = "process #{queue}"

    span_opts = %{kind: :consumer, attributes: attributes}
    span_opts = put_links(span_opts, links)

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, span_opts)
  end

  def handle_job_stop(_event, _measurements, metadata, _config) do
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_job_exception(
        _event,
        _measurements,
        %{stacktrace: stacktrace, error: error} = metadata,
        _config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    Span.record_exception(ctx, error, stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp put_links(span_opts, []), do: span_opts
  defp put_links(span_opts, links), do: Map.put(span_opts, :links, links)

  defp setup_context_propagation(job_meta, :child), do: extract_and_attach(job_meta)
  defp setup_context_propagation(job_meta, :link), do: link_from_propagated_ctx(job_meta)
  defp setup_context_propagation(_job_meta, :none), do: []

  defp extract_and_attach(job_meta) do
    case get_propagated_ctx(job_meta) do
      {_links, parent_ctx} when parent_ctx != :undefined ->
        Ctx.attach(parent_ctx)
        []

      {links, _undefined_ctx} ->
        links
    end
  end

  defp link_from_propagated_ctx(job_meta) do
    {links, _ctx} = get_propagated_ctx(job_meta)
    links
  end

  defp get_propagated_ctx(job_meta) do
    job_meta
    |> Map.to_list()
    |> extract_to_ctx()
  end

  defp extract_to_ctx([]) do
    {[], :undefined}
  end

  defp extract_to_ctx(headers) do
    ctx =
      Ctx.new()
      |> :otel_propagator_text_map.extract_to(headers)

    span_ctx = Tracer.current_span_ctx(ctx)

    case span_ctx do
      :undefined ->
        {[], :undefined}

      span_ctx ->
        {[OpenTelemetry.link(span_ctx)], ctx}
    end
  end
end
