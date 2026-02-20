defmodule OpentelemetryAbsintheTest.SpanKind do
  use OpentelemetryAbsintheTest.Case

  alias OpentelemetryAbsintheTest.Support.GraphQL.Queries
  alias OpentelemetryAbsintheTest.Support.Query

  test "span kind is server" do
    OpentelemetryAbsinthe.Instrumentation.setup()
    :server = Query.query_for_span_kind(Queries.invalid_query())
  end
end
