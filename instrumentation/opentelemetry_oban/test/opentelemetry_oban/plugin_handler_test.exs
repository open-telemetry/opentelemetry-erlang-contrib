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

    assert_receive {:span, span(name: "Elixir.Oban.Plugins.Stager process")}
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
                      events: events,
                      status: ^expected_status
                    )}

    [
      event(
        name: "exception",
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  describe "[:oban, :plugin, :stop] spans" do
    test "Oban.Plugins.Cron plugin" do
      execute_plugin(Oban.Plugins.Cron, %{jobs: [1, 3, 4]})

      assert %{
               "oban.plugin": Elixir.Oban.Plugins.Cron,
               "oban.plugins.cron.jobs_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Cron)
    end

    test "Oban.Plugins.Gossip plugin" do
      execute_plugin(Oban.Plugins.Gossip, %{gossip_count: 3})

      assert %{
               "oban.plugin": Elixir.Oban.Plugins.Gossip,
               "oban.plugins.gossip.gossip_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Gossip)
    end

    test "Oban.Plugins.Lifeline plugin" do
      execute_plugin(Oban.Plugins.Lifeline, %{discarded_count: 3, rescued_count: 2})

      assert %{
               "oban.plugin": Elixir.Oban.Plugins.Lifeline,
               "oban.plugins.lifeline.discarded_count": 3,
               "oban.plugins.lifeline.rescued_count": 2
             } ==
               receive_span_attrs(Oban.Plugins.Lifeline)
    end

    test "Oban.Plugins.Pruner plugin" do
      execute_plugin(Oban.Plugins.Pruner, %{pruned_count: 3})

      assert %{
               "oban.plugin": Elixir.Oban.Plugins.Pruner,
               "oban.plugins.pruner.pruned_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Pruner)
    end

    test "Oban.Pro.Plugins.DynamicCron plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicCron, %{jobs: [1, 3, 4]})

      assert %{
               "oban.plugin": Elixir.Oban.Pro.Plugins.DynamicCron,
               "oban.pro.plugins.dynamic_cron.jobs_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicCron)
    end

    test "Oban.Pro.Plugins.DynamicLifeline plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicLifeline, %{discarded_count: 3, rescued_count: 2})

      assert %{
               "oban.plugin": Elixir.Oban.Pro.Plugins.DynamicLifeline,
               "oban.pro.plugins.dynamic_lifeline.discarded_count": 3,
               "oban.pro.plugins.dynamic_lifeline.rescued_count": 2
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicLifeline)
    end

    test "Oban.Pro.Plugins.DynamicPrioritizer plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicPrioritizer, %{reprioritized_count: 3})

      assert %{
               "oban.plugin": Elixir.Oban.Pro.Plugins.DynamicPrioritizer,
               "oban.pro.plugins.dynamic_prioritizer.reprioritized_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicPrioritizer)
    end

    test "Oban.Pro.Plugins.DynamicPruner plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicPruner, %{pruned_count: 3})

      assert %{
               "oban.plugin": Elixir.Oban.Pro.Plugins.DynamicPruner,
               "oban.pro.plugins.dynamic_pruner.pruned_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicPruner)
    end

    test "Oban.Pro.Plugins.DynamicScaler plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicScaler, %{
        scaler: %{last_scaled_to: 3, last_scaled_at: ~U[2021-08-01 12:00:00Z]}
      })

      assert %{
               "oban.plugin": Elixir.Oban.Pro.Plugins.DynamicScaler,
               "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_to": 3,
               "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_at": "2021-08-01T12:00:00Z"
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicScaler)
    end
  end

  defp receive_span_attrs(name) do
    name = "#{name} process"

    assert_receive(
      {:span, span(name: ^name, attributes: attributes)},
      100,
      "expected span with name #{name} to be received"
    )

    elem(attributes, 4)
  end

  defp execute_plugin(plugin_name, metadata) do
    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: plugin_name}
    )

    :telemetry.execute(
      [:oban, :plugin, :stop],
      %{duration: 42069},
      Map.merge(metadata, %{plugin: plugin_name})
    )
  end
end
