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
    OpentelemetryOban.setup()

    :ok
  end

  test "does not create spans when tracing plugins is disabled" do
    TestHelpers.remove_oban_handlers()
    OpentelemetryOban.setup(trace: [:jobs])

    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Oban.Plugins.Stager}
    )

    :telemetry.execute(
      [:oban, :plugin, :stop],
      %{duration: 444},
      %{plugin: Oban.Plugins.Stager}
    )

    refute_receive {:span, span(name: "Oban.Plugins.Stager process")}
  end

  test "records span on plugin execution" do
    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Oban.Plugins.Stager}
    )

    :telemetry.execute(
      [:oban, :plugin, :stop],
      %{duration: 444},
      %{plugin: Oban.Plugins.Stager}
    )

    assert_receive {:span, span(name: "Oban.Plugins.Stager process")}
  end

  test "records span on plugin error" do
    try do
      :telemetry.span(
        [:oban, :plugin],
        %{plugin: Oban.Plugins.Stager},
        fn ->
          raise "some error"
        end
      )
    rescue
      RuntimeError -> :ok
    end

    expected_status = OpenTelemetry.status(:error, "some error")

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Stager process",
                      events: events,
                      status: ^expected_status
                    )}

    assert [
             event(
               name: "exception",
               attributes: event_attributes
             )
           ] = :otel_events.list(events)

    assert %{
             "exception.type" => "Elixir.RuntimeError",
             "exception.message" => "some error",
             "exception.stacktrace" => _
           } = :otel_attributes.map(event_attributes)
  end
end
