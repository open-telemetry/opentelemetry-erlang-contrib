defmodule OpenTelemetryExAws.DynamoDB do
  alias OpenTelemetry.SemanticConventions.Trace, as: Conventions
  require Conventions

  @attributes %{
    "BatchGetItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.RequestItems", process: &Map.keys/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"}
      ]
    },
    "BatchWriteItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.RequestItems", process: &Map.keys/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"},
        %{attribute: Conventions.aws_dynamodb_item_collection_metrics(), path: "$.ItemCollectionMetrics"}
      ]
    },
    "CreateTable" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_global_secondary_indexes(), path: "$.GlobalSecondaryIndexes"},
        %{attribute: Conventions.aws_dynamodb_local_secondary_indexes(), path: "$.LocalSecondaryIndexes"},
        %{attribute: Conventions.aws_dynamodb_provisioned_read_capacity(), path: "$.ProvisionedThroughput.ReadCapacityUnits"},
        %{attribute: Conventions.aws_dynamodb_provisioned_write_capacity(), path: "$.ProvisionedThroughput.WriteCapacityUnits"},
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"},
        %{attribute: Conventions.aws_dynamodb_item_collection_metrics(), path: "$.ItemCollectionMetrics"}
      ]
    },
    "DeleteItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"},
        %{attribute: Conventions.aws_dynamodb_item_collection_metrics(), path: "$.ItemCollectionMetrics"}
      ]
    },
    "DeleteTable" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ]
    },
    "DescribeTable" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ]
    },
    "GetItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1},
        %{attribute: Conventions.aws_dynamodb_consistent_read(), path: "$.ConsistentRead"},
        %{attribute: Conventions.aws_dynamodb_projection(), path: "$.ProjectionExpression"}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"}
      ]
    },
    "ListTables" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_exclusive_start_table(), path: "$.ExclusiveStartTableName"},
        %{attribute: Conventions.aws_dynamodb_limit(), path: "$.Limit"}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_table_count(), path: "$.TableNames", process: &Enum.count/1}
      ]
    },
    "PutItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.Item", process: &Map.keys/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"},
        %{attribute: Conventions.aws_dynamodb_item_collection_metrics(), path: "$.ItemCollectionMetrics"}
      ]
    },
    "Query" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_scan_forward(), path: "$.ScanIndexForward"},
        %{attribute: Conventions.aws_dynamodb_attributes_to_get(), path: "$.AttributesToGet"},
        %{attribute: Conventions.aws_dynamodb_consistent_read(), path: "$.ConsistentRead"},
        %{attribute: Conventions.aws_dynamodb_index_name(), path: "$.IndexName"},
        %{attribute: Conventions.aws_dynamodb_limit(), path: "$.Limit"},
        %{attribute: Conventions.aws_dynamodb_projection(), path: "$.ProjectionExpression"},
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"}
      ]
    },
    "Scan" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_segment(), path: "$.Segment"},
        %{attribute: Conventions.aws_dynamodb_total_segments(), path: "$.TotalSegments"},
        %{attribute: Conventions.aws_dynamodb_attributes_to_get(), path: "$.AttributesToGet"},
        %{attribute: Conventions.aws_dynamodb_consistent_read(), path: "$.ConsistentRead"},
        %{attribute: Conventions.aws_dynamodb_index_name(), path: "$.IndexName"},
        %{attribute: Conventions.aws_dynamodb_limit(), path: "$.Limit"},
        %{attribute: Conventions.aws_dynamodb_projection(), path: "$.ProjectionExpression"},
        %{attribute: Conventions.aws_dynamodb_select(), path: "$.Select"},
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
        
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_count(), path: "$.Count"},
        %{attribute: Conventions.aws_dynamodb_scanned_count(), path: "$.ScannedCount"},
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"}
      ]
    },
    "UpdateItem" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"},
        %{attribute: Conventions.aws_dynamodb_item_collection_metrics(), path: "$.ItemCollectionMetrics"}
      ]
    },
    "UpdateTable" => %{
      request: [
        %{attribute: Conventions.aws_dynamodb_attribute_definitions(), path: "$.AttributeDefinitions"},
        %{attribute: Conventions.aws_dynamodb_global_secondary_index_updates(), path: "$.GlobalSecondaryIndexUpdates"},
        %{attribute: Conventions.aws_dynamodb_provisioned_read_capacity(), path: "$.ProvisionedThroughput.ReadCapacityUnits"},
        %{attribute: Conventions.aws_dynamodb_provisioned_write_capacity(), path: "$.ProvisionedThroughput.WriteCapacityUnits"},
        %{attribute: Conventions.aws_dynamodb_table_names(), path: "$.TableName", process: &List.wrap/1}
      ],
      response: [
        %{attribute: Conventions.aws_dynamodb_consumed_capacity(), path: "$.ConsumedCapacity"}
      ]
    }
  }

  def get_attributes(%{operation: "DynamoDB" <> _rest} = metadata) do
    [_service, method] = String.split(metadata.operation, ".", parts: 2)

    attribute_definition = Map.get(@attributes, method, %{})

    extract_attributes(metadata.request_body, Map.get(attribute_definition, :request, []))
    |> Map.merge(extract_attributes(metadata.response_body, Map.get(attribute_definition, :response, [])))
    |> Map.put(Conventions.db_system(), "dynamodb")
  end

  defp extract_attributes(_body, []), do: %{}

  defp extract_attributes(body, attribute_definitions) do
    body = Jason.decode!(body)

    Map.new(attribute_definitions, fn %{attribute: attr_key, path: json_path} = definition ->
      process_fun = Map.get(definition, :process, fn x -> x end) 

      val =
        evaluate_json_path(body, json_path)
        |> process_fun.()
        |> Jason.encode!()
    
      {attr_key, val}
    end)
  end

  defp evaluate_json_path(json, path) do
    ["$" | path] = String.split(path, ".")

    get_in(json, path) 
  end
end
