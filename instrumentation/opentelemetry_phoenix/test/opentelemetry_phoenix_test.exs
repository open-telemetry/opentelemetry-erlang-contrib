defmodule OpentelemetryPhoenixTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryPhoenix

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ExceptionAttributes
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
                      name: "NnnnnWeb.MyTestLive.mount",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
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
                      name: "NnnnnWeb.MyTestLive.handle_params",
                      attributes: attributes
                    )}

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
                      name: "NnnnnWeb.MyTestLive.handle_event#hello",
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
                      name: "NnnnnWeb.MyTestLive.mount",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "NnnnnWeb.MyTestLive.handle_params",
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
                      name: "NnnnnWeb.MyTestLive.handle_event#hello",
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
end
