defmodule OpentelemetryAbsintheTest.Configuration do
  use OpentelemetryAbsintheTest.Case

  alias OpentelemetryAbsintheTest.Support.GraphQL.Queries
  alias OpentelemetryAbsintheTest.Support.Query

  @graphql_document :"graphql.document"
  @graphql_operation_name :"graphql.operation.name"
  @graphql_operation_type :"graphql.operation.type"
  @graphql_request_selections :"graphql.request.selections"
  @graphql_event_type :"graphql.event.type"

  doctest OpentelemetryAbsinthe.Instrumentation

  describe "trace configuration" do
    test "doesn't record sensitive data in attributes by default" do
      OpentelemetryAbsinthe.Instrumentation.setup()

      attributes = Query.query_for_attrs(Queries.query(), variables: %{"isbn" => "A1"})

      assert [
               @graphql_document,
               @graphql_event_type,
               @graphql_operation_name,
               @graphql_operation_type,
               @graphql_request_selections
             ] = attributes |> Map.keys() |> Enum.sort()
    end

    test "gives options provided via application env have precedence over defaults" do
      Application.put_env(:opentelemetry_absinthe, :trace_options,
        trace_request_query: false,
        trace_response_result: true
      )

      OpentelemetryAbsinthe.Instrumentation.setup()
      attributes = Query.query_for_attrs(Queries.query(), variables: %{"isbn" => "A1"})

      assert [
               @graphql_event_type,
               @graphql_operation_name,
               @graphql_operation_type,
               @graphql_request_selections,
               :"graphql.response.result"
             ] = attributes |> Map.keys() |> Enum.sort()
    end

    test "gives options provided to setup() precedence over defaults and application env" do
      Application.put_env(:opentelemetry_absinthe, :trace_options,
        trace_request_query: false,
        trace_request_selections: false
      )

      OpentelemetryAbsinthe.Instrumentation.setup(trace_request_query: true)
      attributes = Query.query_for_attrs(Queries.query(), variables: %{"isbn" => "A1"})

      assert [
               @graphql_document,
               @graphql_event_type,
               @graphql_operation_name,
               @graphql_operation_type
             ] = attributes |> Map.keys() |> Enum.sort()
    end
  end
end
