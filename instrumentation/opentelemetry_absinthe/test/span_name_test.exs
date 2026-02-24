defmodule OpentelemetryAbsintheTest.SpanName do
  use OpentelemetryAbsintheTest.Case

  alias OpentelemetryAbsintheTest.Support.GraphQL.Queries
  alias OpentelemetryAbsintheTest.Support.Query

  describe "span name" do
    test "defaults to GraphQL Operation" do
      OpentelemetryAbsinthe.Instrumentation.setup()
      assert "GraphQL Operation" = Query.query_for_span_name(Queries.invalid_query())
    end

    test "is the operation type for an unnamed operation" do
      OpentelemetryAbsinthe.Instrumentation.setup()
      assert "query" = Query.query_for_span_name(Queries.query())
      assert "mutation" = Query.query_for_span_name(Queries.mutation(), variables: %{"isbn" => "A1"})
    end

    test "is the operation type and name for named operations" do
      OpentelemetryAbsinthe.Instrumentation.setup()
      assert "query OperationTwo" = Query.query_for_span_name(Queries.batch_queries(), operation_name: "OperationTwo")
    end

    test "can be overriden with the configuration" do
      span_name = "graphql? I hardly know er"
      OpentelemetryAbsinthe.Instrumentation.setup(span_name: span_name)
      assert ^span_name = Query.query_for_span_name(Queries.batch_queries(), operation_name: "OperationTwo")
    end
  end
end
