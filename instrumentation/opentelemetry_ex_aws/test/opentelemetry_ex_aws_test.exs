defmodule OpentelemetryExAwsTest do
  use OpentelemetryCase, async: false
  doctest OpentelemetryExAws

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

  test "does not crash for unimplemented service calls" do
    OpentelemetryExAws.setup()

    assert {:ok, %{body: %{buckets: []}}} =
             ExAws.S3.list_buckets()
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "s3",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "rpc.service": "s3",
             "rpc.system": "aws-api"
           } = :otel_attributes.map(attributes)
  end
end
