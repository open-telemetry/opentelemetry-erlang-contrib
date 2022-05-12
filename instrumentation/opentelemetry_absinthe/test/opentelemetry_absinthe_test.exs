defmodule OpentelemetryAbsintheTest do
  use ExUnit.Case, async: false

  doctest OpentelemetryAbsinthe

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  defmodule Schema do
    use Absinthe.Schema

    require OpenTelemetry.Tracer

    @organizations 1..3
                   |> Map.new(
                     &{&1,
                      %{
                        id: &1,
                        name: "Organization: ##{&1}"
                      }}
                   )
    @users 1..3
           |> Enum.map(
             &%{
               id: &1,
               name: "User: ##{&1}",
               organization_id: &1
             }
           )

    object :organization do
      field :id, :integer
      field :name, :string
    end

    object :user do
      field :name, :string

      field :organization, :organization do
        resolve fn user, _, _ ->
          batch({__MODULE__, :by_id}, user.organization_id, fn batch ->
            OpenTelemetry.Tracer.with_span "post user.organization batch" do
              {:ok, Map.get(batch, user.organization_id)}
            end
          end)
        end
      end
    end

    query do
      field :users, list_of(:user) do
        resolve fn _, _, _ ->
          async(fn ->
            OpenTelemetry.Tracer.with_span "inside users async" do
              {:ok, @users}
            end
          end)
        end
      end

      field :organization, :organization do
        arg :id, non_null(:integer)

        resolve fn _, %{id: id}, _ ->
          batch({__MODULE__, :by_id}, id, fn batch ->
            OpenTelemetry.Tracer.with_span "post organization batch" do
              {:ok, Map.get(batch, id)}
            end
          end)
        end
      end

      def by_id(_, ids) do
        OpenTelemetry.Tracer.with_span "inside by_id" do
          Map.take(@organizations, ids)
        end
      end
    end
  end

  setup do
    :otel_simple_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)
  end

  describe "graphql.execute" do
    test "query with named operation" do
      OpentelemetryAbsinthe.setup()

      assert {:ok, _result} =
               """
               query ResolverTest {
                 users {
                   name
                 }
               }
               """
               |> Absinthe.run(Schema)

      assert_receive {:span,
                      span(
                        name: :"graphql.execute",
                        span_id: execute_span_id,
                        attributes: attributes
                      )}

      assert %{
               "graphql.operation.name": "ResolverTest",
               "graphql.operation.type": :query,
               "absinthe.schema": "OpentelemetryAbsintheTest.Schema"
             } = :otel_attributes.map(attributes)

      assert_receive {:span,
                      span(
                        name: "RootQueryType.users",
                        parent_span_id: ^execute_span_id,
                        attributes: attributes
                      )}

      assert %{
               "graphql.field.name": "users",
               "graphql.field.path": "users",
               "graphql.field.type": "[User]",
               "absinthe.schema": "OpentelemetryAbsintheTest.Schema"
             } = :otel_attributes.map(attributes)

      refute_receive {:span, span(name: "User.name")}
    end

    test "resolver spans replicate query structure and skip default resolvers" do
      OpentelemetryAbsinthe.setup()

      assert {:ok, _result} =
               """
               query ResolverTest {
                 users {
                   name
                   organization {
                     name
                   }
                 }
               }
               """
               |> Absinthe.run(Schema)

      assert_receive {:span, span(name: :"graphql.execute", span_id: execute_span_id)}

      assert_receive {:span,
                      span(
                        name: "RootQueryType.users",
                        parent_span_id: ^execute_span_id,
                        span_id: user_span_id,
                        attributes: attributes
                      )}

      assert %{
               "graphql.field.name": "users",
               "graphql.field.path": "users",
               "graphql.field.type": "[User]",
               "absinthe.schema": "OpentelemetryAbsintheTest.Schema"
             } = :otel_attributes.map(attributes)

      refute_receive {:span, span(name: "User.name")}

      assert_receive {:span,
                      span(
                        name: "User.organization",
                        parent_span_id: ^user_span_id,
                        attributes: attributes
                      )}

      assert %{
               "graphql.field.name": "organization",
               "graphql.field.path": "users.0.organization",
               "graphql.field.type": "Organization",
               "absinthe.schema": "OpentelemetryAbsintheTest.Schema"
             } = :otel_attributes.map(attributes)

      refute_receive {:span, span(name: "Organization.name")}
    end
  end

  describe "absinthe.middleware.async" do
    test "create spans for helpers and propagate context to async functions" do
      OpentelemetryAbsinthe.setup()

      assert {:ok, _result} =
               """
               query AsyncTest {
                 users {
                   name
                 }
               }
               """
               |> Absinthe.run(Schema)

      assert_receive {:span,
                      span(
                        name: :"graphql.execute",
                        span_id: execute_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "RootQueryType.users",
                        span_id: resolve_span_id,
                        parent_span_id: ^execute_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: :"absinthe.middleware.async",
                        span_id: async_span_id,
                        parent_span_id: ^resolve_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "inside users async",
                        parent_span_id: ^async_span_id
                      )}
    end
  end

  describe "absinthe.middleware.batch" do
    test "create spans for helpers and propagate context to batch functions" do
      OpentelemetryAbsinthe.setup()

      assert {:ok, _result} =
               """
               query BatchTest {
                 organization(id: 1) {
                   name
                 }
               }
               """
               |> Absinthe.run(Schema)

      assert_receive {:span,
                      span(
                        name: :"graphql.execute",
                        span_id: execute_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "RootQueryType.organization",
                        span_id: resolve_span_id,
                        parent_span_id: ^execute_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: :"absinthe.middleware.batch",
                        span_id: batch_span_id,
                        parent_span_id: ^execute_span_id,
                        attributes: attributes
                      )}

      assert %{
               "absinthe.middleware.batch.function": "by_id/2",
               "absinthe.middleware.batch.module": "OpentelemetryAbsintheTest.Schema"
             } = :otel_attributes.map(attributes)

      assert_receive {:span,
                      span(
                        name: :"absinthe.middleware.batch.post",
                        span_id: batch_post_span_id,
                        parent_span_id: ^resolve_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "inside by_id",
                        parent_span_id: ^batch_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "post organization batch",
                        parent_span_id: ^batch_post_span_id
                      )}
    end
  end
end
