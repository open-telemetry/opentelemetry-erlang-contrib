defmodule OpentelemetryOban.InternalHandlerTest do
  use DataCase

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  @events [
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
  ]

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    TestHelpers.remove_oban_handlers()
    OpentelemetryOban.setup(trace: [:internal])

    :ok
  end

  test "does not create spans when internal tracing is disabled" do
    TestHelpers.remove_oban_handlers()
    OpentelemetryOban.setup(trace: [])

    execute_internal_event([:peer, :election])

    refute_receive {:span, span(name: "oban.peer.election")}
  end

  test "records span on internal execution" do
    execute_internal_event([:peer, :election])

    assert_receive {:span, span(name: "oban.peer.election")}
  end

  test "records span on error" do
    :telemetry.execute(
      [:oban, :peer, :election, :start],
      %{system_time: System.system_time()},
      %{}
    )

    exception = %UndefinedFunctionError{
      arity: 0,
      function: :error,
      message: nil,
      module: Some,
      reason: nil
    }

    :telemetry.execute(
      [:oban, :peer, :election, :exception],
      %{duration: 444},
      %{
        kind: :error,
        stacktrace: [
          {Some, :error, [], []}
        ],
        reason: exception
      }
    )

    expected_status = OpenTelemetry.status(:error, Exception.message(exception))

    assert_receive {:span,
                    span(
                      name: "oban.peer.election",
                      events: events,
                      status: ^expected_status
                    )}

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  for event <- @events do
    test "#{inspect([:oban | event])} spans" do
      execute_internal_event(unquote(event))

      assert_receive {:span, span(name: "oban.#{unquote(Enum.join(event, "."))}")}

      :ok
    end
  end

  defp execute_internal_event(event) do
    :telemetry.execute(
      [:oban | event ++ [:start]],
      %{system_time: System.system_time()},
      %{}
    )

    :telemetry.execute(
      [:oban | event ++ [:stop]],
      %{duration: 42069},
      %{}
    )
  end
end
