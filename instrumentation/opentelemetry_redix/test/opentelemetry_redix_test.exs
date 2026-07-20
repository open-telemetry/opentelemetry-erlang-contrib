defmodule OpentelemetryRedixTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryRedix

  alias OpenTelemetry.SemConv.Incubating.DBAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpentelemetryRedix.RedixAttributes

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
      :telemetry.detach({OpentelemetryRedix, :pipeline_stop})
      OpenTelemetry.Tracer.end_span()
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
             DBAttributes.db_operation_name() => "SET",
             DBAttributes.db_query_text() => "SET foo ?",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "records span on mixed-command pipelines" do
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
                      name: "PIPELINE",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "PIPELINE",
             DBAttributes.db_query_text() =>
               "DEL counter\nINCR counter\nINCR counter\nGET counter",
             DBAttributes.db_operation_batch_size() => 4,
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "records span on same-command pipelines" do
    OpentelemetryRedix.setup()

    conn = start_supervised!({Redix, []})

    {:ok, [_, _]} =
      Redix.pipeline(conn, [
        ["INCR", "pipeline_same_cmd_test"],
        ["INCR", "pipeline_same_cmd_test"]
      ])

    assert_receive {:span,
                    span(
                      name: "PIPELINE INCR",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "PIPELINE INCR",
             DBAttributes.db_query_text() =>
               "INCR pipeline_same_cmd_test\nINCR pipeline_same_cmd_test",
             DBAttributes.db_operation_batch_size() => 2,
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "records db.namespace when configured" do
    OpentelemetryRedix.setup(db_namespace: "1")

    conn = start_supervised!({Redix, []})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "SET",
             DBAttributes.db_query_text() => "SET foo ?",
             DBAttributes.db_namespace() => "1",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "omits db.namespace when not configured" do
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
             DBAttributes.db_operation_name() => "SET",
             DBAttributes.db_query_text() => "SET foo ?",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "omits db.query.text when opted out" do
    OpentelemetryRedix.setup(opt_out_attrs: [DBAttributes.db_query_text()])

    conn = start_supervised!({Redix, []})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "SET",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost"
           } == :otel_attributes.map(attributes)
  end

  test "includes redix.connection.name when opted in" do
    OpentelemetryRedix.setup(opt_in_attrs: [RedixAttributes.redix_connection_name()])

    conn = start_supervised!({Redix, [name: :my_conn]})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "SET",
             DBAttributes.db_query_text() => "SET foo ?",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost",
             RedixAttributes.redix_connection_name() => :my_conn
           } == :otel_attributes.map(attributes)
  end

  test "merges extra_attrs onto spans" do
    OpentelemetryRedix.setup(extra_attrs: %{:"custom.attr" => "value"})

    conn = start_supervised!({Redix, []})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             DBAttributes.db_operation_name() => "SET",
             DBAttributes.db_query_text() => "SET foo ?",
             :"db.system.name" => "redis",
             ServerAttributes.server_address() => "localhost",
             :"custom.attr" => "value"
           } == :otel_attributes.map(attributes)
  end

  test "instrumented attributes take precedence over extra_attrs" do
    OpentelemetryRedix.setup(extra_attrs: %{DBAttributes.db_operation_name() => "OVERRIDDEN"})

    conn = start_supervised!({Redix, []})

    {:ok, "OK"} = Redix.command(conn, ["SET", "foo", "bar"])

    assert_receive {:span,
                    span(
                      name: "SET",
                      kind: :client,
                      attributes: attributes
                    )}

    attrs = :otel_attributes.map(attributes)
    assert attrs[DBAttributes.db_operation_name()] == "SET"
  end
end
