defmodule OpentelemetryPhoenixTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryPhoenix

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  alias PhoenixMeta, as: Meta

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
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :application.start(:opentelemetry)

    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())
    :ok
  end

  test "records spans for Phoenix web requests" do
    OpentelemetryPhoenix.setup()

    :telemetry.execute(
      [:phoenix, :endpoint, :start],
      %{system_time: System.system_time()},
      Meta.endpoint_start()
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :start],
      %{system_time: System.system_time()},
      Meta.router_dispatch_start()
    )

    :telemetry.execute(
      [:phoenix, :endpoint, :stop],
      %{duration: 444},
      Meta.endpoint_stop()
    )

    assert_receive {:span,
                    span(
                      name: "/users/:user_id",
                      attributes: attributes,
                      parent_span_id: 13_235_353_014_750_950_193
                    )}

    assert %{
             "http.client_ip": "10.211.55.2",
             "http.flavor": :"1.1",
             "http.host": "localhost",
             "http.method": "GET",
             "http.route": "/users/:user_id",
             "http.scheme": "http",
             "http.status_code": 200,
             "http.target": "/users/123",
             "http.user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0",
             "net.host.ip": "10.211.55.2",
             "net.host.port": 4000,
             "net.peer.ip": "10.211.55.2",
             "net.peer.port": 64291,
             "net.transport": :"IP.TCP",
             "phoenix.action": :user,
             "phoenix.plug": Elixir.MyStoreWeb.PageController
           } == :otel_attributes.map(attributes)
  end

  test "parses x-forwarded-for with single value" do
    OpentelemetryPhoenix.setup()

    x_forwarded_for_request("203.0.113.195")

    assert_receive {:span, span(attributes: attributes)}

    assert Map.fetch!(:otel_attributes.map(attributes), :"http.client_ip") == "203.0.113.195"
  end

  test "parses x-forwarded-for with multiple values" do
    OpentelemetryPhoenix.setup()

    x_forwarded_for_request("203.0.113.195, 70.41.3.18, 150.172.238.178")

    assert_receive {:span, span(attributes: attributes)}

    assert Map.fetch!(:otel_attributes.map(attributes), :"http.client_ip") == "203.0.113.195"
  end

  test "records exceptions for Phoenix web requests" do
    OpentelemetryPhoenix.setup()

    :telemetry.execute(
      [:phoenix, :endpoint, :start],
      %{system_time: System.system_time()},
      Meta.endpoint_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :start],
      %{system_time: System.system_time()},
      Meta.router_dispatch_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :exception],
      %{duration: 222},
      Meta.router_dispatch_exception(:normal)
    )

    :telemetry.execute(
      [:phoenix, :endpoint, :stop],
      %{duration: 444},
      Meta.endpoint_stop(:exception)
    )

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "/users/:user_id/exception",
                      attributes: attributes,
                      kind: :server,
                      events: events,
                      parent_span_id: 13_235_353_014_750_950_193,
                      status: ^expected_status
                    )}

    assert %{
             "http.client_ip": "10.211.55.2",
             "http.flavor": :"1.1",
             "http.host": "localhost",
             "http.method": "GET",
             "http.route": "/users/:user_id/exception",
             "http.scheme": "http",
             "http.status_code": 500,
             "http.target": "/users/123/exception",
             "http.user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0",
             "net.host.ip": "10.211.55.2",
             "net.host.port": 4000,
             "net.peer.ip": "10.211.55.2",
             "net.peer.port": 64291,
             "net.transport": :"IP.TCP",
             "phoenix.action": :code_exception,
             "phoenix.plug": MyStoreWeb.PageController
           } == :otel_attributes.map(attributes)

    [
      event(
        name: "exception",
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:key, :map, "exception.message", "exception.stacktrace", "exception.type"] ==
             Map.keys(:otel_attributes.map(event_attributes))
  end

  test "records exceptions for nested Phoenix routers" do
    OpentelemetryPhoenix.setup()

    :telemetry.execute(
      [:phoenix, :endpoint, :start],
      %{system_time: System.system_time()},
      Meta.endpoint_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :start],
      %{system_time: System.system_time()},
      Meta.router_dispatch_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :exception],
      %{duration: 222},
      Meta.router_dispatch_exception(:normal)
    )

    :telemetry.execute(
      [:phoenix, :endpoint, :stop],
      %{duration: 444},
      Meta.endpoint_stop(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :exception],
      %{duration: 222},
      Meta.router_dispatch_exception(:normal)
    )

    assert_receive {:span, _}

    assert [_ | _] = :telemetry.list_handlers([:phoenix, :router_dispatch, :exception])
  end

  test "records exceptions for Phoenix web requests with plug wrappers" do
    OpentelemetryPhoenix.setup()

    :telemetry.execute(
      [:phoenix, :endpoint, :start],
      %{system_time: System.system_time()},
      Meta.endpoint_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :start],
      %{system_time: System.system_time()},
      Meta.router_dispatch_start(:exception)
    )

    :telemetry.execute(
      [:phoenix, :router_dispatch, :exception],
      %{duration: 222},
      Meta.router_dispatch_exception(:plug_wrapper)
    )

    :telemetry.execute(
      [:phoenix, :endpoint, :stop],
      %{duration: 444},
      Meta.endpoint_stop(:exception)
    )

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "/users/:user_id/exception",
                      attributes: attributes,
                      kind: :server,
                      events: events,
                      parent_span_id: 13_235_353_014_750_950_193,
                      status: ^expected_status
                    )}

    assert %{
             "http.client_ip": "10.211.55.2",
             "http.flavor": :"1.1",
             "http.host": "localhost",
             "http.method": "GET",
             "http.route": "/users/:user_id/exception",
             "http.scheme": "http",
             "http.status_code": 500,
             "http.target": "/users/123/exception",
             "http.user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:81.0) Gecko/20100101 Firefox/81.0",
             "net.host.ip": "10.211.55.2",
             "net.host.port": 4000,
             "net.peer.ip": "10.211.55.2",
             "net.peer.port": 64291,
             "net.transport": :"IP.TCP",
             "phoenix.action": :code_exception,
             "phoenix.plug": MyStoreWeb.PageController
           } == :otel_attributes.map(attributes)

    [
      event(
        name: "exception",
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert ["exception.message", "exception.stacktrace", "exception.type"] ==
             Map.keys(:otel_attributes.map(event_attributes))
  end

  defp x_forwarded_for_request(x_forwarded_for) do
    meta = Meta.endpoint_start()

    meta = %{
      meta
      | conn: %{
          meta.conn
          | req_headers: [{"x-forwarded-for", x_forwarded_for} | meta.conn.req_headers]
        }
    }

    :telemetry.execute(
      [:phoenix, :endpoint, :start],
      %{system_time: System.system_time()},
      meta
    )

    :telemetry.execute(
      [:phoenix, :endpoint, :stop],
      %{duration: 444},
      Meta.endpoint_stop()
    )
  end
end
