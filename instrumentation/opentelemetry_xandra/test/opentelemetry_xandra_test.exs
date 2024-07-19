defmodule OpentelemetryXandraTest do
  use ExUnit.Case, async: false

  require OpenTelemetry.Tracer
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecordp(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecordp(name, spec)
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)
  end

  describe "span creation when executing queries" do
    test "when the query is successful" do
      OpentelemetryXandra.setup()

      conn = start_supervised!({Xandra, connect_timeout: 5_000})

      Xandra.execute!(conn, "SELECT * FROM system.local", [])

      assert_receive {:span, span(name: "SELECT") = span}

      assert span(span, :kind) == :client
      assert span(span, :status) == OpenTelemetry.status(:ok)

      attributes = :otel_attributes.map(span(span, :attributes))
      assert attributes[:"db.system"] == "cassandra"
      assert attributes[:"db.operation"] == "SELECT"
      assert attributes[:"server.address"] == "127.0.0.1"
      assert attributes[:"network.peer.address"] == "127.0.0.1"
      assert attributes[:"network.peer.port"] == 9042
    end

    test "when the query is a prepared query" do
      OpentelemetryXandra.setup()

      conn = start_supervised!({Xandra, connect_timeout: 5_000})

      prepared = Xandra.prepare!(conn, "SELECT * FROM system.local")
      Xandra.execute!(conn, prepared, [])

      assert_receive {:span, span(name: "SELECT") = span}

      assert span(span, :kind) == :client
      assert span(span, :status) == OpenTelemetry.status(:ok)

      attributes = :otel_attributes.map(span(span, :attributes))
      assert attributes[:"db.system"] == "cassandra"
      assert attributes[:"db.operation"] == "SELECT"
      assert attributes[:"server.address"] == "127.0.0.1"
      assert attributes[:"network.peer.address"] == "127.0.0.1"
      assert attributes[:"network.peer.port"] == 9042
    end

    test "with the :operation_parser option" do
      OpentelemetryXandra.setup(operation_parser: fn _query -> {"SELECT", nil, nil} end)

      conn = start_supervised!({Xandra, connect_timeout: 5000})

      Xandra.execute!(conn, "SELECT * FROM system.local", [])

      assert_receive {:span, span(name: "SELECT") = span}

      assert span(span, :kind) == :client
      assert span(span, :status) == OpenTelemetry.status(:ok)

      attributes = :otel_attributes.map(span(span, :attributes))
      assert attributes[:"db.system"] == "cassandra"
      assert attributes[:"db.operation"] == "SELECT"
      assert attributes[:"server.address"] == "127.0.0.1"
      assert attributes[:"network.peer.address"] == "127.0.0.1"
      assert attributes[:"network.peer.port"] == 9042
    end
  end
end
