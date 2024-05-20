defmodule OpentelemetryExAws.DynamoDbTest do
  use OpentelemetryCase, async: false

  @table_name "test_table"

  setup do
    OpentelemetryExAws.setup()

    delete_test_table!()

    []
  end

  test "fallback for not instrumented aws dynamodb operation" do
    create_test_table!()

    ExAws.Dynamo.update_time_to_live(@table_name, "ttl", true)
    |> ExAws.request()
    |> case do
      {:ok, _result} ->
        :ok

      {:error, {"ValidationException", "TimeToLive is already enabled"}} ->
        :ok

      {:error, reason} ->
        raise "Failed to set TTL settings #{inspect(reason)}"
    end

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.UpdateTimeToLive",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "UpdateTimeToLive",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api"
           } == :otel_attributes.map(attributes)
  end

  test "does not crash on failed AWS API call" do
    assert {:error, _} =
             ExAws.Dynamo.delete_table("this_table_does_not_exist")
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.DeleteTable",
                      kind: :client
                    )}

    assert {:ok, _} = ExAws.Dynamo.list_tables() |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.ListTables",
                      kind: :client
                    )}
  end

  defp delete_test_table!() do
    ExAws.Dynamo.delete_table(@table_name)
    |> ExAws.request()
    |> case do
      {:ok, _result} ->
        :ok

      {:error, {"ResourceNotFoundException", "Cannot do operations on a non-existent table"}} ->
        :ok

      {:error, reason} ->
        raise "Failed to delete DynamoDB table #{inspect(reason)}"
    end
  end

  defp create_test_table!() do
    ExAws.Dynamo.create_table(
      @table_name,
      [{"partition_key", :hash}, {"attribute_name", :range}],
      [{:partition_key, :string}, {:attribute_name, :string}],
      read_capacity: 1,
      write_capacity: 1
    )
    |> ExAws.request()
    |> case do
      {:ok, _result} ->
        :ok

      {:error, {"ResourceInUseException", "Cannot create preexisting table"}} ->
        :ok

      {:error, reason} ->
        raise "Failed to create DynamoDB table #{inspect(reason)}"
    end
  end
end
