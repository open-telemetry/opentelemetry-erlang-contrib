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

  test "BatchWriteItem" do
    create_test_table!()

    assert {:ok, _} =
             ExAws.Dynamo.batch_write_item(%{
               @table_name =>
                 [
                   %{"partition_key" => "key1", "attribute_name" => "value1"},
                   %{"partition_key" => "key2", "attribute_name" => "value2"}
                 ]
                 |> Enum.map(&[{:put_request, [{:item, &1}]}])
             })
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.BatchWriteItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "BatchWriteItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.item_collection_metrics": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "BatchGetItem" do
    create_test_table!()

    data = [
      %{"partition_key" => "key1", "attribute_name" => "value1"},
      %{"partition_key" => "key1", "attribute_name" => "value3"},
      %{"partition_key" => "key2", "attribute_name" => "value2"}
    ]

    {:ok, _} =
      ExAws.Dynamo.batch_write_item(%{
        @table_name => Enum.map(data, &[{:put_request, [{:item, &1}]}])
      })
      |> ExAws.request()

    assert {:ok, %{"Responses" => %{"test_table" => response_data}}} =
             ExAws.Dynamo.batch_get_item(%{
               @table_name => [
                 {:keys,
                  [
                    [{:partition_key, "key1"}, {:attribute_name, "value1"}],
                    [{:partition_key, "key2"}, {:attribute_name, "value2"}],
                    [{:partition_key, "key1"}, {:attribute_name, "value3"}]
                  ]}
               ]
             })
             |> ExAws.request()

    assert data == Enum.map(response_data, &ExAws.Dynamo.Decoder.decode/1)

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.BatchGetItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "BatchGetItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "CreateTable" do
    create_test_table!()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.CreateTable",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "CreateTable",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.global_secondary_indexes": "null",
             "aws.dynamodb.item_collection_metrics": "null",
             "aws.dynamodb.local_secondary_indexes": "null",
             "aws.dynamodb.provisioned_read_capacity": "1",
             "aws.dynamodb.provisioned_write_capacity": "1",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "DeleteItem" do
    create_test_table!()

    data = [
      %{"partition_key" => "key1", "attribute_name" => "value1"},
      %{"partition_key" => "key1", "attribute_name" => "value3"},
      %{"partition_key" => "key2", "attribute_name" => "value2"}
    ]

    {:ok, _} =
      ExAws.Dynamo.batch_write_item(%{
        @table_name => Enum.map(data, &[{:put_request, [{:item, &1}]}])
      })
      |> ExAws.request()

    assert {:ok, _} =
             ExAws.Dynamo.delete_item(
               @table_name,
               [{:partition_key, "key1"}, {:attribute_name, "value1"}]
             )
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.DeleteItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "DeleteItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.item_collection_metrics": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "DeleteTable" do
    create_test_table!()

    assert {:ok, _} =
             ExAws.Dynamo.delete_table(@table_name)
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.DeleteTable",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "DeleteTable",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "GetItem" do
    create_test_table!()

    data = %{"partition_key" => "key1", "attribute_name" => "value1"}

    {:ok, _} =
      ExAws.Dynamo.put_item(@table_name, data)
      |> ExAws.request()

    assert {:ok, %{"Item" => response_data}} =
             ExAws.Dynamo.get_item(
               @table_name,
               [
                 {:partition_key, "key1"},
                 {:attribute_name, "value1"}
               ],
               consistent_read: true,
               projection_expression: "attribute_name"
             )
             |> ExAws.request()

    assert %{"attribute_name" => "value1"} == ExAws.Dynamo.Decoder.decode(response_data)

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.GetItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "GetItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consistent_read": "true",
             "aws.dynamodb.projection": "\"attribute_name\"",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "ListTables" do
    create_test_table!()

    assert {:ok, %{"TableNames" => table_names}} = ExAws.Dynamo.list_tables() |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.ListTables",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "ListTables",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.exclusive_start_table": "null",
             "aws.dynamodb.limit": "null",
             "aws.dynamodb.table_count": "1"
           } == :otel_attributes.map(attributes)

    assert Enum.member?(table_names, @table_name)
  end

  test "PutItem" do
    create_test_table!()

    data = %{"partition_key" => "key1", "attribute_name" => "value1"}

    assert {:ok, _} =
             ExAws.Dynamo.put_item(@table_name, data)
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.PutItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "PutItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.item_collection_metrics": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "Query" do
    create_test_table!()

    data = [
      %{"partition_key" => "key1", "attribute_name" => "value1"},
      %{"partition_key" => "key1", "attribute_name" => "value3"},
      %{"partition_key" => "key2", "attribute_name" => "value2"}
    ]

    {:ok, _} =
      ExAws.Dynamo.batch_write_item(%{
        @table_name => Enum.map(data, &[{:put_request, [{:item, &1}]}])
      })
      |> ExAws.request()

    assert {:ok, %{"Items" => response_data}} =
             ExAws.Dynamo.query(
               @table_name,
               consistent_read: true,
               key_condition_expression: "partition_key = :partition_key",
               expression_attribute_values: [partition_key: "key1"],
               scan_index_forward: true,
               select: :all_attributes,
               limit: 2
             )
             |> ExAws.request()

    assert [
             %{"attribute_name" => "value1", "partition_key" => "key1"},
             %{"attribute_name" => "value3", "partition_key" => "key1"}
           ] ==
             Enum.map(response_data, &ExAws.Dynamo.Decoder.decode/1)

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.Query",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "aws.dynamodb.attributes_to_get": "null",
             "aws.dynamodb.consistent_read": "true",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.index_name": "null",
             "aws.dynamodb.limit": "2",
             "aws.dynamodb.projection": "null",
             "aws.dynamodb.scan_forward": "true",
             "aws.dynamodb.table_names": "[\"test_table\"]",
             "db.system": "dynamodb",
             "rpc.method": "Query",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api"
           } == :otel_attributes.map(attributes)
  end

  test "Scan" do
    create_test_table!()

    data = [
      %{"partition_key" => "key1", "attribute_name" => "value1", "other_attribute" => "1"},
      %{"partition_key" => "key1", "attribute_name" => "value3", "other_attribute" => "1"},
      %{"partition_key" => "key2", "attribute_name" => "value2", "other_attribute" => "1"}
    ]

    {:ok, _} =
      ExAws.Dynamo.batch_write_item(%{
        @table_name => Enum.map(data, &[{:put_request, [{:item, &1}]}])
      })
      |> ExAws.request()

    assert {:ok, %{"Items" => response_data}} =
             ExAws.Dynamo.scan(
               @table_name,
               consistent_read: true,
               filter_expression: "other_attribute= :other_attribute",
               expression_attribute_values: [other_attribute: "1"],
               projection_expression: "partition_key, attribute_name",
               scan_index_forward: false,
               limit: 1
             )
             |> ExAws.request()

    assert [
             %{"attribute_name" => "value2", "partition_key" => "key2"}
           ] ==
             Enum.map(response_data, &ExAws.Dynamo.Decoder.decode/1)

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.Scan",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "aws.dynamodb.attributes_to_get": "null",
             "aws.dynamodb.consistent_read": "true",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.limit": "1",
             "aws.dynamodb.projection": "\"partition_key, attribute_name\"",
             "aws.dynamodb.table_names": "[\"test_table\"]",
             "db.system": "dynamodb",
             "rpc.method": "Scan",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.count": "1",
             "aws.dynamodb.index_name": "null",
             "aws.dynamodb.scanned_count": "1",
             "aws.dynamodb.segment": "null",
             "aws.dynamodb.select": "null",
             "aws.dynamodb.total_segments": "null"
           } == :otel_attributes.map(attributes)
  end

  test "UpdateItem" do
    create_test_table!()

    data = %{"partition_key" => "key1", "attribute_name" => "value1", "other_attribute" => "1"}

    {:ok, _} =
      ExAws.Dynamo.put_item(@table_name, data)
      |> ExAws.request()

    assert {:ok, _} =
             ExAws.Dynamo.update_item(
               @table_name,
               [{:partition_key, "key1"}, {:attribute_name, "value1"}],
               update_expression: "SET other_attribute = :other_attribute",
               expression_attribute_values: [other_attribute: "10"]
             )
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.UpdateItem",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "db.system": "dynamodb",
             "rpc.method": "UpdateItem",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.consumed_capacity": "null",
             "aws.dynamodb.item_collection_metrics": "null",
             "aws.dynamodb.table_names": "[\"test_table\"]"
           } == :otel_attributes.map(attributes)
  end

  test "UpdateTable" do
    create_test_table!()

    assert {:ok, _} =
             ExAws.Dynamo.update_table(
               @table_name,
               global_indexes: %{"A" => "B"},
               read_capacity: 2,
               write_capacity: 2,
               billing_mode: :pay_per_request
             )
             |> ExAws.request()

    assert_receive {:span,
                    span(
                      name: "DynamoDB_20120810.UpdateTable",
                      kind: :client,
                      attributes: attributes
                    )}

    assert %{
             "aws.dynamodb.global_secondary_index_updates": "{\"A\":\"B\"}",
             "aws.dynamodb.provisioned_read_capacity": "2",
             "aws.dynamodb.provisioned_write_capacity": "2",
             "aws.dynamodb.table_names": "[\"test_table\"]",
             "db.system": "dynamodb",
             "rpc.method": "UpdateTable",
             "rpc.service": "DynamoDB_20120810",
             "rpc.system": "aws-api",
             "aws.dynamodb.attribute_definitions": "null",
              "aws.dynamodb.consumed_capacity": "null"
           } == :otel_attributes.map(attributes)
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
