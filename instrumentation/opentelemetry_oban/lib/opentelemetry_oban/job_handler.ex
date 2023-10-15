defmodule OpentelemetryOban.JobHandler do
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemanticConventions.Trace

  require Trace

  @tracer_id __MODULE__

  def attach(config) do
    attach_job_start_handler()
    attach_job_stop_handler(config)
    attach_job_exception_handler(config)
  end

  defp attach_job_start_handler() do
    :telemetry.attach(
      "#{__MODULE__}.job_start",
      [:oban, :job, :start],
      &__MODULE__.handle_job_start/4,
      []
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

  def handle_job_start(_event, _measurements, metadata, _config) do
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

    :otel_propagator_text_map.extract(Map.to_list(job_meta))
    parent = OpenTelemetry.Tracer.current_span_ctx()
    links = if parent == :undefined, do: [], else: [OpenTelemetry.link(parent)]
    OpenTelemetry.Tracer.set_current_span(:undefined)

    attributes = %{
      Trace.messaging_system() => :oban,
      Trace.messaging_destination() => queue,
      Trace.messaging_destination_kind() => :queue,
      Trace.messaging_operation() => :process,
      :"messaging.oban.job_id" => id,
      :"messaging.oban.worker" => worker,
      :"messaging.oban.priority" => priority,
      :"messaging.oban.attempt" => attempt,
      :"messaging.oban.max_attempts" => max_attempts,
      :"messaging.oban.inserted_at" =>
        if(inserted_at, do: DateTime.to_iso8601(inserted_at), else: nil),
      :"messaging.oban.scheduled_at" => DateTime.to_iso8601(scheduled_at)
    }

    span_name = "#{worker} process"

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, span_name, metadata, %{
      kind: :consumer,
      links: links,
      attributes: attributes
    })
  end

  def handle_job_stop(_event, measurements, metadata, config) do
    set_measurements_attributes(measurements, config)
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  def handle_job_exception(
        _event,
        measurements,
        %{stacktrace: stacktrace, error: error} = metadata,
        config
      ) do
    ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    # Record exception and mark the span as errored
    Span.record_exception(ctx, error, stacktrace)
    Span.set_status(ctx, OpenTelemetry.status(:error, ""))

    set_measurements_attributes(measurements, config)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp set_measurements_attributes(%{duration: duration, queue_time: queue_time}, %{
         time_unit: time_unit
       }) do
    OpenTelemetry.Tracer.set_attributes(%{
      :"messaging.oban.duration_#{time_unit}" =>
        System.convert_time_unit(duration, :native, time_unit),
      :"messaging.oban.queue_time_#{time_unit}" =>
        System.convert_time_unit(queue_time, :nanosecond, time_unit)
    })
  end
end
