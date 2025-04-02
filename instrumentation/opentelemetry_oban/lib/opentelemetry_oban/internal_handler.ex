defmodule OpentelemetryOban.InternalHandler do
  alias OpenTelemetry.Span
  alias OpenTelemetry.SemanticConventions.Trace

  require Trace

  @tracer_id __MODULE__

  def attach() do
    :telemetry.attach_many(
      {__MODULE__, :internal},
      Enum.flat_map(
        [
          [:engine, :init],
          [:engine, :refresh],
          [:engine, :put_meta],
          [:engine, :check_available],
          [:engine, :cancel_all_jobs],
          [:engine, :fetch_jobs],
          [:engine, :insert_all_jobs],
          [:engine, :prune_all_jobs],
          [:engine, :stage_jobs],
          [:engine, :cancel_job],
          [:engine, :complete_job],
          [:engine, :discard_job],
          [:engine, :error_job],
          [:engine, :insert_job],
          [:engine, :snooze_job],
          [:notifier, :notify],
          [:peer, :election]
        ],
        fn event ->
          [
            [:oban | event ++ [:start]],
            [:oban | event ++ [:stop]],
            [:oban | event ++ [:exception]]
          ]
        end
      ),
      &__MODULE__.handle_oban_event/4,
      []
    )
  end

  def handle_oban_event(event, _measurements, metadata, _config) do
    [op | rest] = Enum.reverse(event)

    case op do
      :start ->
        OpentelemetryTelemetry.start_telemetry_span(__MODULE__, Enum.join(Enum.reverse(rest), "."), metadata, %{kind: :consumer})

      :stop ->
        OpentelemetryTelemetry.end_telemetry_span(__MODULE__, metadata)

      :exception ->
        ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

        Span.record_exception(ctx, metadata.reason, metadata.stacktrace)
        Span.set_status(ctx, OpenTelemetry.status(:error, Exception.message(metadata.reason)))

        OpentelemetryTelemetry.end_telemetry_span(__MODULE__, metadata)
    end
  end
end
