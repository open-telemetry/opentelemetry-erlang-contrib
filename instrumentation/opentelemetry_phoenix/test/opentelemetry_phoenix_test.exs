defmodule OpentelemetryPhoenixTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryPhoenix

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias OpenTelemetry.SemConv.ExceptionAttributes

  @socket Phoenix.LiveView.Utils.configure_socket(
            %Phoenix.LiveView.Socket{
              endpoint: NnnnWeb.Endpoint,
              router: NnnnnWeb.Router,
              view: NnnnnWeb.MyTestLive
            },
            %{
              connect_params: %{},
              connect_info: %{},
              root_view: NnnnnWeb.MyTestLive,
              live_temp: %{}
            },
            nil,
            %{},
            URI.parse("https://localhost:4000/live?foo=bar")
          )

  @telemetry_meta_base %{
    socket: @socket,
    params: %{"foo" => "bar"},
    uri: "https://localhost:4000/live?foo=bar",
    telemetry_span_context: :dummy_ref
  }

  @telemetry_meta_handle_params Map.put(@telemetry_meta_base, :event, "hello")

  @exception_meta %{
    reason: %RuntimeError{message: "stop"},
    stacktrace: [
      {NnnnnWeb.MyTestLive, :handle_params, 3,
       [
         file: ~c"lib/nnnnn_web/live/my_test_live.ex",
         line: 28,
         error_info: %{module: Exception}
       ]}
    ],
    kind: :error
  }

  @telemetry_meta %{
    mount: %{
      start: @telemetry_meta_base,
      stop: @telemetry_meta_base,
      exception: @telemetry_meta_base
    },
    handle_params: %{
      start: @telemetry_meta_base,
      stop: @telemetry_meta_base,
      exception: Map.merge(@telemetry_meta_base, @exception_meta)
    },
    handle_event: %{
      start: @telemetry_meta_handle_params,
      stop: @telemetry_meta_handle_params,
      exception: Map.merge(@telemetry_meta_handle_params, @exception_meta)
    }
  }

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpentelemetryPhoenix.setup(adapter: :cowboy2)

    on_exit(fn ->
      Enum.each(:telemetry.list_handlers([]), &:telemetry.detach(&1.id))
    end)

    :ok
  end

  test "records spans for Phoenix LiveView mount" do
    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.mount.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      @telemetry_meta.mount.stop
    )

    assert_receive {:span,
                    span(
                      name: "NnnnnWeb.MyTestLive.mount",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
  end

  test "records spans for Phoenix LiveView handle_params" do
    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_params.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :stop],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_params.stop
    )

    assert_receive {:span,
                    span(
                      name: "NnnnnWeb.MyTestLive.handle_params",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
  end

  test "records spans for Phoenix LiveView handle_event" do
    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_event.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :stop],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_event.stop
    )

    assert_receive {:span,
                    span(
                      name: "NnnnnWeb.MyTestLive.handle_event#hello",
                      attributes: attributes
                    )}

    assert %{} == :otel_attributes.map(attributes)
  end

  test "handles exception during Phoenix LiveView handle_params" do
    :telemetry.execute(
      [:phoenix, :live_view, :mount, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.mount.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :mount, :stop],
      %{system_time: System.system_time()},
      @telemetry_meta.mount.stop
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_params.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_params, :exception],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_params.exception
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
    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :start],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_event.start
    )

    :telemetry.execute(
      [:phoenix, :live_view, :handle_event, :exception],
      %{system_time: System.system_time()},
      @telemetry_meta.handle_event.exception
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
