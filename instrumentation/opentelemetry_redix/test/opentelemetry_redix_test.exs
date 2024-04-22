defmodule OpentelemetryRedixTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryRedix

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
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
      :telemetry.detach({OpentelemetryRedix, :pipeline_stop})
    end)
  end

  test "records span on commands" do
    OpentelemetryRedix.setup()

    conn = start_supervised!({Redix, []})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.operation": "SET",
             "db.statement": "SET foo ?",
             "db.system": "redis",
             "net.peer.name": "localhost",
             "net.peer.port": "6379"
           } = :otel_attributes.map(attributes)
  end

  test "records span on piplines" do
    OpentelemetryRedix.setup()

    conn = start_supervised!({Redix, []})

    {:ok, [_, _, _, "2"]} =
      Redix.pipeline(conn, [
        ["DEL", "counter"],
        ["INCR", "counter"],
        ["INCR", "counter"],
        ["GET", "counter"]
      ])

    assert_receive {:span,
                    span(
                      name: "pipeline",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.operation": "pipeline",
             "db.statement": "DEL counter\nINCR counter\nINCR counter\nGET counter",
             "db.system": "redis",
             "net.peer.name": "localhost",
             "net.peer.port": "6379"
           } = :otel_attributes.map(attributes)
  end

  test "records span without db.statement if configured not to" do
    OpentelemetryRedix.setup(db_statement: false)

    conn = start_supervised!({Redix, []})

    {:ok, [_]} =
      Redix.pipeline(conn, [
        ["GET", "counter"]
      ])

    assert_receive {:span,
                    span(
                      name: "GET",
                      kind: :client,
                      attributes: attributes
                    )}

    refute Map.has_key?(:otel_attributes.map(attributes), :"db.statement")
  end
end
