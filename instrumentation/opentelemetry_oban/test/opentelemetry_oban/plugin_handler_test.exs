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
    OpentelemetryOban.setup(plugin: :disabled)

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

    refute_receive {:span, span(name: "Oban.Plugins.Stager process")}
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

    assert_receive {:span, span(name: "Oban.Plugins.Stager process")}
  end

  test "records span on plugin error" do
    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Elixir.Oban.Plugins.Stager}
    )

    exception = %UndefinedFunctionError{
      arity: 0,
      function: :error,
      message: "function Some.error/0 is undefined (module Some is not available)",
      module: Some,
      reason: nil
    }

    stacktrace = [{Some, :error, [], []}]

    :telemetry.execute(
      [:oban, :plugin, :exception],
      %{duration: 444},
      %{
        plugin: Elixir.Oban.Plugins.Stager,
        kind: :error,
        stacktrace: stacktrace,
        reason: exception
      }
    )

    expected_status =
      OpenTelemetry.status(:error, Exception.format_banner(:error, exception, stacktrace))

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Stager process",
                      attributes: span_attributes,
                      events: events,
                      status: ^expected_status
                    )}

    assert %{"error.type": "UndefinedFunctionError"} =
             :otel_attributes.map(span_attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert %{
             "exception.type": "Elixir.UndefinedFunctionError",
             "exception.message":
               "function Some.error/0 is undefined (module Some is not available)"
           } = :otel_attributes.map(event_attributes)
  end

  test "records span on plugin error with non-exception reason" do
    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Elixir.Oban.Plugins.Stager}
    )

    # `:badarg` normalises to %ArgumentError{}, so it records a message like any other exception
    stacktrace = [{Some, :error, [], []}]

    :telemetry.execute(
      [:oban, :plugin, :exception],
      %{duration: 444},
      %{
        plugin: Elixir.Oban.Plugins.Stager,
        kind: :error,
        stacktrace: stacktrace,
        reason: :badarg
      }
    )

    expected_status =
      OpenTelemetry.status(:error, Exception.format_banner(:error, :badarg, stacktrace))

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Stager process",
                      attributes: span_attributes,
                      events: events,
                      status: ^expected_status
                    )}

    assert %{"error.type": "ArgumentError"} = :otel_attributes.map(span_attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert %{
             "exception.type": "Elixir.ArgumentError",
             "exception.message": "argument error"
           } = :otel_attributes.map(event_attributes)
  end

  test "records span on plugin error with a reason that cannot be normalised" do
    :telemetry.execute(
      [:oban, :plugin, :start],
      %{system_time: System.system_time()},
      %{plugin: Elixir.Oban.Plugins.Stager}
    )

    stacktrace = [{Some, :error, [], []}]

    :telemetry.execute(
      [:oban, :plugin, :exception],
      %{duration: 444},
      %{
        plugin: Elixir.Oban.Plugins.Stager,
        kind: :throw,
        stacktrace: stacktrace,
        reason: {:some, :value}
      }
    )

    expected_status =
      OpenTelemetry.status(:error, Exception.format_banner(:throw, {:some, :value}, stacktrace))

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Stager process",
                      attributes: span_attributes,
                      events: events,
                      status: ^expected_status
                    )}

    refute Map.has_key?(:otel_attributes.map(span_attributes), :"error.type")

    [event(name: :exception, attributes: event_attributes)] = :otel_events.list(events)

    # The Erlang API accepts any term but records no message
    assert [:"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  # Plugins report failure through the `{:error, meta}` return value of `:telemetry.span/3`, which
  # emits a `:stop` event rather than an `:exception` event.
  test "marks the span as errored when a stop event carries a wrapped :error" do
    # Oban.Plugins.{Cron,Lifeline,Pruner,Reindexer} put their whole return value under :error
    execute_plugin(Oban.Plugins.Cron, %{error: {:error, %RuntimeError{message: "insert failed"}}})

    expected_status = OpenTelemetry.status(:error, "insert failed")

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Cron process",
                      attributes: span_attributes,
                      status: ^expected_status
                    )}

    assert %{"error.type": "RuntimeError"} = :otel_attributes.map(span_attributes)
  end

  test "marks the span as errored when a stop event carries a bare :error" do
    # Oban.Stager destructures {:error, reason} and puts the bare reason under :error
    execute_plugin(Oban.Stager, %{error: %RuntimeError{message: "notify failed"}})

    expected_status = OpenTelemetry.status(:error, "notify failed")

    assert_receive {:span,
                    span(
                      name: "Oban.Stager process",
                      attributes: span_attributes,
                      status: ^expected_status
                    )}

    assert %{"error.type": "RuntimeError"} = :otel_attributes.map(span_attributes)
  end

  test "leaves the span unset when a stop event carries a nil :error" do
    # Oban.Plugins.Reindexer has no else branch for the non-leader case, so every follower node
    # reports a nil :error on each scheduled run. That is a no-op, not a failure.
    execute_plugin(Oban.Plugins.Reindexer, %{error: nil})

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Reindexer process",
                      attributes: span_attributes,
                      status: :undefined
                    )}

    refute Map.has_key?(:otel_attributes.map(span_attributes), :"error.type")
  end

  describe "[:oban, :plugin, :stop] spans" do
    test "Oban.Plugins.Cron plugin" do
      execute_plugin(Oban.Plugins.Cron, %{jobs: [1, 3, 4]})

      assert %{
               "oban.plugin": "Oban.Plugins.Cron",
               "oban.plugins.cron.jobs_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Cron)
    end

    test "Oban.Plugins.Cron plugin without :jobs in metadata" do
      # Oban omits :jobs from the [:oban, :plugin, :stop] metadata when the
      # scheduled insert fails (Oban.Plugins.Cron returns {:error, meta} with an
      # :error key and no :jobs). jobs_count must default to 0 rather than crash
      # the telemetry handler on length(nil), which would detach it.
      execute_plugin(Oban.Plugins.Cron, %{
        error: {:error, %RuntimeError{message: "insert failed"}}
      })

      assert %{
               "error.type": "RuntimeError",
               "oban.plugin": "Oban.Plugins.Cron",
               "oban.plugins.cron.jobs_count": 0
             } ==
               receive_span_attrs(Oban.Plugins.Cron)
    end

    test "Oban.Plugins.Gossip plugin" do
      execute_plugin(Oban.Plugins.Gossip, %{gossip_count: 3})

      assert %{
               "oban.plugin": "Oban.Plugins.Gossip",
               "oban.plugins.gossip.gossip_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Gossip)
    end

    test "Oban.Plugins.Lifeline plugin" do
      execute_plugin(Oban.Plugins.Lifeline, %{
        discarded_jobs: [1, 2, 3],
        rescued_jobs: [4, 5]
      })

      assert %{
               "oban.plugin": "Oban.Plugins.Lifeline",
               "oban.plugins.lifeline.discarded_count": 3,
               "oban.plugins.lifeline.rescued_count": 2
             } ==
               receive_span_attrs(Oban.Plugins.Lifeline)
    end

    test "Oban.Plugins.Pruner plugin" do
      execute_plugin(Oban.Plugins.Pruner, %{pruned_count: 3})

      assert %{
               "oban.plugin": "Oban.Plugins.Pruner",
               "oban.plugins.pruner.pruned_count": 3
             } ==
               receive_span_attrs(Oban.Plugins.Pruner)
    end

    test "Oban.Pro.Plugins.DynamicCron plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicCron, %{jobs: [1, 3, 4]})

      assert %{
               "oban.plugin": "Oban.Pro.Plugins.DynamicCron",
               "oban.pro.plugins.dynamic_cron.jobs_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicCron)
    end

    test "Oban.Pro.Plugins.DynamicCron plugin without :jobs in metadata" do
      execute_plugin(Oban.Pro.Plugins.DynamicCron, %{
        error: {:error, %RuntimeError{message: "insert failed"}}
      })

      assert %{
               "error.type": "RuntimeError",
               "oban.plugin": "Oban.Pro.Plugins.DynamicCron",
               "oban.pro.plugins.dynamic_cron.jobs_count": 0
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicCron)
    end

    test "Oban.Pro.Plugins.DynamicLifeline plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicLifeline, %{
        discarded_jobs: [1, 2, 3],
        rescued_jobs: [4, 5]
      })

      assert %{
               "oban.plugin": "Oban.Pro.Plugins.DynamicLifeline",
               "oban.pro.plugins.dynamic_lifeline.discarded_count": 3,
               "oban.pro.plugins.dynamic_lifeline.rescued_count": 2
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicLifeline)
    end

    test "Oban.Pro.Plugins.DynamicPrioritizer plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicPrioritizer, %{reprioritized_count: 3})

      assert %{
               "oban.plugin": "Oban.Pro.Plugins.DynamicPrioritizer",
               "oban.pro.plugins.dynamic_prioritizer.reprioritized_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicPrioritizer)
    end

    test "Oban.Pro.Plugins.DynamicPruner plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicPruner, %{pruned_count: 3})

      assert %{
               "oban.plugin": "Oban.Pro.Plugins.DynamicPruner",
               "oban.pro.plugins.dynamic_pruner.pruned_count": 3
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicPruner)
    end

    test "Oban.Pro.Plugins.DynamicScaler plugin" do
      execute_plugin(Oban.Pro.Plugins.DynamicScaler, %{
        scaler: %{last_scaled_to: 3, last_scaled_at: ~U[2021-08-01 12:00:00Z]}
      })

      assert %{
               "oban.plugin": "Oban.Pro.Plugins.DynamicScaler",
               "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_to": 3,
               "oban.pro.plugins.dynamic_scaler.scaler.last_scaled_at": "2021-08-01T12:00:00Z"
             } ==
               receive_span_attrs(Oban.Pro.Plugins.DynamicScaler)
    end
  end

  defp receive_span_attrs(name) do
    name = "#{inspect(name)} process"

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
