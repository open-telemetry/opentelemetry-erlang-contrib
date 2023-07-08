defmodule OpentelemetryExAwsTest do
  use ExUnit.Case
  doctest OpentelemetryExAws

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
    end)

    :ok
  end

  test "records span" do
    OpentelemetryExAws.setup()

    op = %ExAws.Operation.JSON{
      http_method: :post,
      service: :dynamodb,
      headers: [
        {"x-amz-target", "DynamoDB_20120810.ListTables"},
        {"content-type", "application/x-amz-json-1.0"}
      ]
    }

    assert {:ok, %{"TableNames" => _}} = ExAws.request(op)

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.ListTables",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "rpc.method": "ListTables",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api"
           } = :otel_attributes.map(attributes)
  end
end
