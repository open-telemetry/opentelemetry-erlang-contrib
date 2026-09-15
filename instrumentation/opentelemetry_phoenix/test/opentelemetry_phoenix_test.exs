defmodule OpentelemetryPhoenixTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryPhoenix

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ExceptionAttributes
  alias OpenTelemetry.SemConv.Incubating.HTTPAttributes
  alias PhoenixLiveViewMeta, as: LiveViewMeta

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    Application.ensure_all_started([:telemetry])
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    on_exit(fn ->
      :telemetry.list_handlers([])
      |> Enum.each(fn h -> :telemetry.detach(h.id) end)
    end)

    :ok
  end

  test "records spans for Phoenix LiveView mount" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.mount",
                      attributes: attributes
                    )}

    assert %{HTTPAttributes.http_route() => "/live"} ==
             :otel_attributes.map(attributes)
  end

  test "records spans for Phoenix LiveView handle_params" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_params_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_params_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.handle_params",
                      attributes: attributes
                    )}

    assert %{HTTPAttributes.http_route() => "/live"} ==
             :otel_attributes.map(attributes)
  end

  test "records the route template for a parameterized LiveView route" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    meta = put_uri(LiveViewMeta.mount_start(), "http://localhost:4000/resources/123?foo=bar")

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      meta
    )

    assert_receive {:span, span(attributes: attributes)}

    assert %{HTTPAttributes.http_route() => "/resources/:resource_id"} ==
             :otel_attributes.map(attributes)
  end

  test "omits the route when the LiveView is not mounted at the router" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    meta = put_uri(LiveViewMeta.mount_start(), nil)

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      meta
    )

    assert_receive {:span, span(attributes: attributes)}
    assert %{} == :otel_attributes.map(attributes)

    assert Enum.any?(
             :telemetry.list_handlers([:phoenix, :live_view, :mount, :start]),
             &(&1.id == {OpentelemetryPhoenix, :live_view})
           )
  end

  test "omits the route when the socket has no router" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    meta = LiveViewMeta.mount_start()
    meta = %{meta | socket: %{meta.socket | router: nil}}

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      meta
    )

    assert_receive {:span, span(attributes: attributes)}
    assert %{} == :otel_attributes.map(attributes)
  end

  test "omits the route when the path matches no route" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    meta = put_uri(LiveViewMeta.mount_start(), "http://localhost:4000/nope")

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      meta
    )

    assert_receive {:span, span(attributes: attributes)}
    assert %{} == :otel_attributes.map(attributes)
  end

  test "records spans for Phoenix LiveView handle_event" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_event_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_event_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.handle_event#hello",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
  end

  test "handles exception during Phoenix LiveView handle_params" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_stop(:exception)
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_params_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :exception],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_params_exception(:exception)
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.mount",
                      attributes: attributes
                    )}

    assert %{HTTPAttributes.http_route() => "/live"} ==
             :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.handle_params",
                      attributes: attributes,
                      events: events
                    )}

    assert %{HTTPAttributes.http_route() => "/live"} ==
             :otel_attributes.map(attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "handles exceptions during Phoenix LiveView handle_event" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_event_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :exception],
      %{system_time: System.system_time()},
      LiveViewMeta.handle_event_exception(:exception)
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.handle_event#hello",
                      attributes: attributes,
                      events: events
                    )}

    assert %{} == :otel_attributes.map(attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] == Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "records spans for Phoenix LiveView render" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :render, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.render_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.render_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.render",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
  end

  test "handles exception during Phoenix LiveView render" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :render, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.render_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :exception],
      %{system_time: System.system_time()},
      LiveViewMeta.render_exception()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.render",
                      attributes: attributes,
                      events: events
                    )}

    assert %{} == :otel_attributes.map(attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] == Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "records spans for Phoenix LiveComponent render" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :render, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.component_render_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.component_render_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.MyLiveComponent.render",
                      attributes: attributes
                    )}

    attrs = :otel_attributes.map(attributes)
    assert attrs[:"live_view.module"] == "OpentelemetryPhoenix.TestSupport.MyTestLive"
  end

  test "handles exception during Phoenix LiveComponent render" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_view, :render, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.component_render_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :exception],
      %{system_time: System.system_time()},
      LiveViewMeta.component_render_exception()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.MyLiveComponent.render",
                      attributes: attributes,
                      events: events
                    )}

    attrs = :otel_attributes.map(attributes)
    assert attrs[:"live_view.module"] == "OpentelemetryPhoenix.TestSupport.MyTestLive"

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] == Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "records spans for Phoenix LiveComponent update" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_component, :update, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.component_update_start()
    )

    :telemetry.execute(
      [:phoenix, :live_component, :update, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.component_update_stop()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.MyLiveComponent.update",
                      attributes: attributes
                    )}

    attrs = :otel_attributes.map(attributes)
    assert attrs[:"live_view.module"] == "OpentelemetryPhoenix.TestSupport.MyTestLive"
  end

  test "handles exception during Phoenix LiveComponent update" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    :telemetry.execute(
      [:phoenix, :live_component, :update, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.component_update_start()
    )

    :telemetry.execute(
      [:phoenix, :live_component, :update, :exception],
      %{system_time: System.system_time()},
      LiveViewMeta.component_update_exception()
    )

    assert_receive {:span,
                    span(
                      name: "OpentelemetryPhoenix.TestSupport.MyTestLive.MyLiveComponent.update",
                      attributes: attributes,
                      events: events
                    )}

    attrs = :otel_attributes.map(attributes)
    assert attrs[:"live_view.module"] == "OpentelemetryPhoenix.TestSupport.MyTestLive"

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [
             ExceptionAttributes.exception_message(),
             ExceptionAttributes.exception_stacktrace(),
             ExceptionAttributes.exception_type()
           ] == Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "does not record LiveView spans when liveview: false" do
    OpentelemetryPhoenix.setup(adapter: :cowboy2, liveview: false)

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.mount_stop()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :start],
      %{system_time: System.system_time()},
      LiveViewMeta.render_start()
    )

    :telemetry.execute(
      [:phoenix, :live_view, :render, :stop],
      %{system_time: System.system_time()},
      LiveViewMeta.render_stop()
    )

    refute_receive {:span, _}
  end

  defp put_uri(meta, uri), do: %{meta | uri: uri}
end
