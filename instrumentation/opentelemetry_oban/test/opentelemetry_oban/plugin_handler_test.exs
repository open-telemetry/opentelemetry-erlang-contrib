defmodule OpentelemetryOban.PluginHandlerTest do
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

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    TestHelpers.remove_oban_handlers()

    :ok
  end

  describe "with the default config" do
    setup do
      OpentelemetryOban.setup()
    end

    test "does not create spans when tracing plugins is disabled" do
      TestHelpers.remove_oban_handlers()
      OpentelemetryOban.setup(trace: [:jobs])

      :telemetry.execute(
        [:oban, :plugin, :start],
        %{system_time: System.system_time()},
        %{plugin: Elixir.Oban.Plugins.Stager}
      )

      :telemetry.execute(
        [:oban, :plugin, :stop],
        %{duration: 444},
        %{plugin: Elixir.Oban.Plugins.Stager}
      )

      refute_receive {:span, span(name: "Elixir.Oban.Plugins.Stager process")}
    end

    test "records span on plugin execution" do
      :telemetry.execute(
        [:oban, :plugin, :start],
        %{system_time: System.system_time()},
        %{plugin: Elixir.Oban.Plugins.Stager}
      )

      :telemetry.execute(
        [:oban, :plugin, :stop],
        %{duration: 444},
        %{plugin: Elixir.Oban.Plugins.Stager}
      )

      assert_receive {:span,
                      span(name: "Elixir.Oban.Plugins.Stager process", attributes: attributes)}

      assert %{
               "messaging.oban.duration_microsecond": _duration
             } = :otel_attributes.map(attributes)
    end

    test "records span on plugin error" do
      :telemetry.execute(
        [:oban, :plugin, :start],
        %{system_time: System.system_time()},
        %{plugin: Elixir.Oban.Plugins.Stager}
      )

      :telemetry.execute(
        [:oban, :plugin, :exception],
        %{duration: 444},
        %{
          plugin: Elixir.Oban.Plugins.Stager,
          kind: :error,
          stacktrace: [
            {Some, :error, [], []}
          ],
          error: %UndefinedFunctionError{
            arity: 0,
            function: :error,
            message: nil,
            module: Some,
            reason: nil
          }
        }
      )

      expected_status = OpenTelemetry.status(:error, "")

      assert_receive {:span,
                      span(
                        name: "Elixir.Oban.Plugins.Stager process",
                        attributes: attributes,
                        events: events,
                        status: ^expected_status
                      )}

      assert %{
               "messaging.oban.duration_microsecond": _duration
             } = :otel_attributes.map(attributes)

      [
        event(
          name: "exception",
          attributes: event_attributes
        )
      ] = :otel_events.list(events)

      assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
               Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
    end
  end

  test "can configure time_unit" do
    OpentelemetryOban.setup(time_unit: :second)

    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Elixir.Oban.Plugins.Stager}
    )

    :telemetry.execute(
      [:oban, :plugin, :stop],
      %{duration: 444},
      %{plugin: Elixir.Oban.Plugins.Stager}
    )

    assert_receive {:span,
                    span(name: "Elixir.Oban.Plugins.Stager process", attributes: attributes)}

    assert %{
             "messaging.oban.duration_second": _duration
           } = :otel_attributes.map(attributes)
  end
end
