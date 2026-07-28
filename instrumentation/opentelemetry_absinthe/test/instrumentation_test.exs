defmodule OpentelemetryAbsintheTest.InstrumentationTest do
  use OpentelemetryAbsintheTest.Case

  alias OpentelemetryAbsinthe.Instrumentation
  alias OpentelemetryAbsinthe.TelemetryMetadata
  alias OpentelemetryAbsintheTest.Support.GraphQL.Queries
  alias OpentelemetryAbsintheTest.Support.Query

  @capture_all [
    trace_request_query: true,
    trace_request_variables: true,
    trace_response_result: true,
    trace_response_errors: true,
    trace_request_selections: true
  ]

  @trace_attributes [
    :"graphql.document",
    :"graphql.event.type",
    :"graphql.operation.name",
    :"graphql.operation.type",
    :"graphql.request.selections",
    :"graphql.request.variables",
    :"graphql.response.errors",
    :"graphql.response.result"
  ]

  test "doesn't crash when query is empty" do
    OpentelemetryAbsinthe.Instrumentation.setup(@capture_all)
    attrs = Query.query_for_attrs(Queries.empty_query())

    assert @trace_attributes = attrs |> Map.keys() |> Enum.sort()
  end

  test "handles multiple queries properly" do
    OpentelemetryAbsinthe.Instrumentation.setup(@capture_all)
    attrs = Query.query_for_attrs(Queries.batch_queries(), variables: %{"isbn" => "A1"}, operation_name: "OperationOne")

    assert @trace_attributes = attrs |> Map.keys() |> Enum.sort()
  end

  describe "error_status option" do
    test "default (:all) marks span as error when resolver returns error" do
      Instrumentation.setup()
      status = Query.query_for_status(Queries.failing_query())

      assert {:status, :error, ""} = status
    end

    test "default (:all) does not mark span as error when resolver succeeds" do
      Instrumentation.setup()
      status = Query.query_for_status(Queries.query(), variables: %{"isbn" => "A1"})

      assert :undefined = status
    end

    test ":none never marks span as error even when resolver returns error" do
      Instrumentation.setup(error_status: :none)
      status = Query.query_for_status(Queries.failing_query())

      assert :undefined = status
    end

    test "custom function can selectively mark errors" do
      classifier = fn errors ->
        has_not_found? =
          Enum.any?(errors, fn
            %{message: "not found"} -> true
            _ -> false
          end)

        if has_not_found?, do: :error, else: :ok
      end

      Instrumentation.setup(error_status: classifier)
      status = Query.query_for_status(Queries.failing_query())

      assert {:status, :error, ""} = status
    end

    test "custom function can suppress known errors" do
      classifier = fn _errors -> :ok end

      Instrumentation.setup(error_status: classifier)
      status = Query.query_for_status(Queries.failing_query())

      assert :undefined = status
    end

    test "error_status can be configured via application env" do
      Application.put_env(:opentelemetry_absinthe, :trace_options, error_status: :none)
      Instrumentation.setup()
      status = Query.query_for_status(Queries.failing_query())

      assert :undefined = status
    end

    test "error_status passed to setup/1 takes precedence over application env" do
      Application.put_env(:opentelemetry_absinthe, :trace_options, error_status: :none)
      Instrumentation.setup(error_status: :all)
      status = Query.query_for_status(Queries.failing_query())

      assert {:status, :error, ""} = status
    end
  end

  describe "handles metadata" do
    setup do
      config =
        Instrumentation.default_config()
        |> Keyword.put(:type, :operation)
        |> Enum.into(%{})

      %{config: config}
    end

    test "standard values are returned when no metadata in context", ctx do
      assert :ok =
               :telemetry.attach(
                 ctx.test,
                 [:opentelemetry_absinthe, :graphql, :handled],
                 fn _telemetry_event, _measurements, metadata, _config ->
                   send(self(), metadata)
                 end,
                 nil
               )

      assert :ok =
               Instrumentation.handle_stop(
                 "Test",
                 %{},
                 %{blueprint: BlueprintArchitect.blueprint(schema: __MODULE__)},
                 ctx.config
               )

      assert_receive %{
                       operation_name: "TestOperation",
                       operation_type: :query,
                       schema: __MODULE__,
                       errors: nil,
                       status: :ok
                     },
                     10
    end

    test "standard values are returned alongside the metadata from context", ctx do
      context = TelemetryMetadata.update_context(%{}, %{source: "TestSource", user_agent: "Insomnia"})

      blueprint =
        BlueprintArchitect.blueprint(schema: __MODULE__, execution: BlueprintArchitect.execution(context: context))

      assert :ok =
               :telemetry.attach(
                 ctx.test,
                 [:opentelemetry_absinthe, :graphql, :handled],
                 fn _telemetry_event, _measurements, metadata, _config ->
                   send(self(), metadata)
                 end,
                 nil
               )

      assert :ok = Instrumentation.handle_stop("Test", %{}, %{blueprint: blueprint}, ctx.config)

      assert_receive %{
                       operation_name: "TestOperation",
                       operation_type: :query,
                       schema: __MODULE__,
                       errors: nil,
                       status: :ok,
                       user_agent: "Insomnia",
                       source: "TestSource"
                     },
                     10
    end
  end
end
