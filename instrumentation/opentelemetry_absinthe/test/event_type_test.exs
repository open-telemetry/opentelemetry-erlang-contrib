defmodule OpentelemetryAbsintheTest.EventTypeTest do
  use OpentelemetryAbsintheTest.Case

  alias OpentelemetryAbsintheTest.Support.GraphQL.Queries
  alias OpentelemetryAbsintheTest.Support.Query

  alias OpentelemetryAbsintheTest.EventTypeTest.TelemetryProvider

  @graphql_event_type :"graphql.event.type"

  doctest OpentelemetryAbsinthe.Instrumentation

  test "records operation on query" do
    OpentelemetryAbsinthe.Instrumentation.setup()

    attributes = Query.query_for_attrs(Queries.query(), variables: %{"isbn" => "A1"})

    assert @graphql_event_type in Map.keys(attributes)
    assert :operation == Map.get(attributes, @graphql_event_type)
  end

  describe "subscription" do
    test "doesn't attach to subscription if not enabled" do
      Application.put_env(:opentelemetry_absinthe, :telemetry_provider, TelemetryProvider)

      on_exit(fn ->
        Application.put_env(:opentelemetry_absinthe, :telemetry_provider, :telemetry)
      end)

      {:ok, _} = TelemetryProvider.start_link([])

      OpentelemetryAbsinthe.Instrumentation.setup()

      # Attaches only to operation start/stop
      assert 2 == TelemetryProvider.count()
    end

    test "attaches to subscription if enabled" do
      Application.put_env(:opentelemetry_absinthe, :telemetry_provider, TelemetryProvider)

      on_exit(fn ->
        Application.put_env(:opentelemetry_absinthe, :telemetry_provider, :telemetry)
      end)

      {:ok, _} = TelemetryProvider.start_link([])

      OpentelemetryAbsinthe.Instrumentation.setup(trace_subscriptions: true)

      # Attaches also to subscription start/stop
      assert 4 == TelemetryProvider.count()
    end
  end
end

defmodule OpentelemetryAbsintheTest.EventTypeTest.TelemetryProvider do
  use Agent

  @me __MODULE__

  def start_link(_opts) do
    Agent.start_link(fn -> 0 end, name: @me)
  end

  def count, do: Agent.get(@me, & &1)

  defp increment, do: Agent.update(@me, &(&1 + 1))

  def attach(_, _, _, _), do: increment()
end
