defmodule OpentelemetryEctoTest do
  use ExUnit.Case
  import Ecto.Query
  require OpenTelemetry.Tracer

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.DBAttributes
  alias OpenTelemetry.SemConv.ServerAttributes
  alias OpentelemetryEcto.EctoAttributes

  alias OpentelemetryEcto.TestRepo, as: Repo
  alias OpentelemetryEcto.MyXQLTestRepo, as: MyXQLRepo
  alias OpentelemetryEcto.Sqlite3TestRepo, as: Sqlite3Repo
  alias OpentelemetryEcto.TdsTestRepo, as: TdsRepo
  alias OpentelemetryEcto.TestModels.{Comment, User, Post}

  require Ecto.Query, as: Query
  require OpenTelemetry.Tracer, as: Tracer

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  setup_all do
    # this is done to capture init events when the repos get started
    setup_instrumentation()

    OpentelemetryEcto.TestRepo.start_link()
    OpentelemetryEcto.TestRepo.Replica1.start_link()
    OpentelemetryEcto.MyXQLTestRepo.start_link()
    OpentelemetryEcto.TdsTestRepo.start_link()
    OpentelemetryEcto.Sqlite3TestRepo.start_link()

    Ecto.Migrator.with_repo(OpentelemetryEcto.TestRepo, &Ecto.Migrator.run(&1, :up, all: true))

    Ecto.Migrator.with_repo(
      OpentelemetryEcto.TestRepo.Replica1,
      &Ecto.Migrator.run(&1, :up, all: true)
    )

    Ecto.Migrator.with_repo(
      OpentelemetryEcto.MyXQLTestRepo,
      &Ecto.Migrator.run(&1, :up, all: true)
    )

    Ecto.Migrator.with_repo(OpentelemetryEcto.TdsTestRepo, &Ecto.Migrator.run(&1, :up, all: true))

    Ecto.Migrator.with_repo(
      OpentelemetryEcto.Sqlite3TestRepo,
      &Ecto.Migrator.run(&1, :up, all: true)
    )

    ExUnit.start(capture_log: true)

    Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.TestRepo, {:shared, self()})
    Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.TestRepo.Replica1, {:shared, self()})

    Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.MyXQLTestRepo, {:shared, self()})
    Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.TdsTestRepo, {:shared, self()})
    Ecto.Adapters.SQL.Sandbox.mode(OpentelemetryEcto.Sqlite3TestRepo, {:shared, self()})

    clear_handlers()
  end

  setup do
    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())

    on_exit(fn ->
      clear_handlers()

      Repo.delete_all(Comment)
      Repo.delete_all(Post)
      Repo.delete_all(User)
    end)
  end

  defp setup_instrumentation(opts \\ []) do
    [
      event_prefix: [:opentelemetry_ecto, :test_repo]
    ]
    |> Keyword.merge(opts)
    |> OpentelemetryEcto.setup()
  end

  defp clear_handlers do
    :telemetry.list_handlers([])
    |> Enum.reject(&match?(%{id: {OpentelemetryEcto, :init}}, &1))
    |> Enum.each(fn h -> :telemetry.detach(h.id) end)
  end

  test "dynamic repo" do
    setup_instrumentation()
    setup_instrumentation(event_prefix: [:tenant, :test])

    {:ok, dyn_pid} =
      start_supervised({
        OpentelemetryEcto.TestRepo,
        name: :tenant_test, hostname: "0.0.0.0", port: 5433, telemetry_prefix: [:tenant, :test]
      })

    start_supervised({
      OpentelemetryEcto.TestRepo,
      name: nil, hostname: "0.0.0.0", port: 5433
    })

    start_supervised({
      OpentelemetryEcto.TestRepo,
      name: nil, hostname: "127.0.0.1", port: 5433
    })

    Ecto.Adapter.lookup_meta(Repo.get_dynamic_repo())

    Repo.all(User, telemetry_options: [foo: :bar])

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    refute Map.has_key?(attrs, ServerAttributes.server_port()),
           "port is not set when default for db"

    Repo.put_dynamic_repo(:tenant_test)

    Repo.__adapter__().storage_up(Repo.config())

    Ecto.Migrator.with_repo(Repo, &Ecto.Migrator.run(&1, :up, all: true, dynamic_repo: dyn_pid))
    setup_instrumentation(event_prefix: [:tenant, :test])
    Ecto.Adapter.lookup_meta(Repo.get_dynamic_repo())

    Ecto.Adapter.lookup_meta(Repo.Replica1.get_dynamic_repo())

    Repo.all(User)

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    assert Map.get(attrs, ServerAttributes.server_port()) == 5433,
           "port is set when not default for db"
  end

  test "captures basic query events - postgres" do
    setup_instrumentation()

    Repo.all(User)

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    expected_attrs =
      [
        EctoAttributes.ecto_decode_time_duration(),
        EctoAttributes.ecto_query_time_duration(),
        EctoAttributes.ecto_queue_time_duration(),
        EctoAttributes.ecto_total_time_duration()
      ]

    for attr <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert is_float(actual), "#{attr} expected a float got #{inspect(actual)}"
    end

    expected_attrs = [
      {DBAttributes.db_system(), DBAttributes.db_system_values().postgresql},
      {DBAttributes.db_collection_name(), "users"},
      {DBAttributes.db_namespace(), "opentelemetry_ecto_test"},
      {ServerAttributes.server_address(), "localhost"},
      {DBAttributes.db_query_text(), ~s(SELECT u0."id", u0."email" FROM "users" AS u0)},
      {DBAttributes.db_operation_name(), :SELECT}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{actual}"
    end
  end

  test "captures basic query events - myxql" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :my_xql_test_repo])

    MyXQLRepo.all(User)

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    expected_attrs =
      [
        EctoAttributes.ecto_decode_time_duration(),
        EctoAttributes.ecto_query_time_duration(),
        EctoAttributes.ecto_queue_time_duration(),
        EctoAttributes.ecto_total_time_duration()
      ]

    for attr <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert is_float(actual), "#{attr} expected a float got #{inspect(actual)}"
    end

    expected_attrs = [
      {DBAttributes.db_system(), DBAttributes.db_system_values().mysql},
      {DBAttributes.db_collection_name(), "users"},
      {DBAttributes.db_namespace(), "opentelemetry_ecto_test"},
      {ServerAttributes.server_address(), "localhost"},
      {DBAttributes.db_query_text(), "SELECT u0.`id`, u0.`email` FROM `users` AS u0"},
      {DBAttributes.db_operation_name(), :SELECT}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{actual}"
    end
  end

  test "captures basic query events - tds" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :tds_test_repo])

    TdsRepo.all(User)

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    expected_attrs =
      [
        EctoAttributes.ecto_decode_time_duration(),
        EctoAttributes.ecto_query_time_duration(),
        EctoAttributes.ecto_queue_time_duration(),
        EctoAttributes.ecto_total_time_duration()
      ]

    for attr <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert is_float(actual), "#{attr} expected a float got #{inspect(actual)}"
    end

    expected_attrs = [
      {DBAttributes.db_system(), DBAttributes.db_system_values().mssql},
      {DBAttributes.db_collection_name(), "users"},
      {DBAttributes.db_namespace(), "opentelemetry_ecto_test"},
      {ServerAttributes.server_address(), "localhost"},
      {DBAttributes.db_query_text(), "SELECT u0.[id], u0.[email] FROM [users] AS u0"},
      {DBAttributes.db_operation_name(), :SELECT}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{actual}"
    end
  end

  test "captures basic query events - sqlite3" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :sqlite3_test_repo])

    Sqlite3Repo.all(User)

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attrs = :otel_attributes.map(attributes)

    expected_attrs =
      [
        EctoAttributes.ecto_decode_time_duration(),
        EctoAttributes.ecto_query_time_duration(),
        EctoAttributes.ecto_queue_time_duration(),
        EctoAttributes.ecto_total_time_duration()
      ]

    for attr <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert is_float(actual), "#{attr} expected a float got #{inspect(actual)}"
    end

    expected_attrs = [
      {DBAttributes.db_system(), DBAttributes.db_system_values().sqlite},
      {DBAttributes.db_collection_name(), "users"},
      {DBAttributes.db_namespace(), "opentelemetry_ecto_test.db"},
      {ServerAttributes.server_address(), "opentelemetry_ecto_test.db"},
      {DBAttributes.db_query_text(), ~s(SELECT u0."id", u0."email" FROM "users" AS u0)},
      {DBAttributes.db_operation_name(), :SELECT}
    ]

    for {attr, expected} <- expected_attrs do
      actual = Map.get(attrs, attr)
      assert expected == actual, "#{attr} expected #{expected} got #{actual}"
    end
  end

  test "allow per-request options via telemetry_options" do
    setup_instrumentation(additional_span_attributes: %{"config.attribute": "special value", "db.system": "my_system"})

    Repo.all(User,
      telemetry_options: [
        otel: %{
          span_name: "custom span name",
          attributes: %{
            "config.attribute": "special value overwritten",
            "db.system": "my_system",
            extra: "should add"
          }
        }
      ]
    )

    assert_receive {:span, span(name: "custom span name", attributes: attributes)}

    # don't merge instrumentation-set attrs but can overwrite config set
    assert %{
             "config.attribute": "special value overwritten",
             "db.system": :postgresql,
             extra: "should add"
           } =
             :otel_attributes.map(attributes)
  end

  def custom_metadata_processor(meta) do
    %{meta | query: "custom sanitized"}
  end

  test "allows modification of metadata" do
    setup_instrumentation(telemetry_metadata_preprocessor: &__MODULE__.custom_metadata_processor/1)

    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert "custom sanitized" == :otel_attributes.map(attributes)[DBAttributes.db_query_text()]
  end

  test "include additional_attributes" do
    setup_instrumentation(additional_span_attributes: %{"config.attribute": "special value", "db.system": "my_system"})

    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    # don't merge instrumentation-set attrs
    assert %{"config.attribute": "special value", "db.system": :postgresql} =
             :otel_attributes.map(attributes)
  end

  test "collects multiple spans" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})

    setup_instrumentation()

    User
    |> Repo.all()
    |> Repo.preload([:posts])

    assert_receive {:span, span(name: "SELECT users")}
    assert_receive {:span, span(name: "SELECT posts")}
  end

  test "sets error message on error - postgres" do
    setup_instrumentation()

    try do
      Repo.all(from(u in "users", select: u.non_existent_field))
    rescue
      _ -> :ok
    end

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      status: {:status, :error, message},
                      attributes: attributes
                    )}

    assert message =~ "non_existent_field does not exist"

    assert %{unquote(ErrorAttributes.error_type()) => :undefined_column} =
             :otel_attributes.map(attributes)
  end

  test "sets error message on error - myxql" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :my_xql_test_repo])

    try do
      MyXQLRepo.all(from(u in "users", select: u.non_existent_field))
    rescue
      _ -> :ok
    end

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      status: {:status, :error, message},
                      attributes: attributes
                    )}

    assert message =~ "Unknown column 'u0.non_existent_field'"

    assert %{unquote(ErrorAttributes.error_type()) => 1054} =
             :otel_attributes.map(attributes)
  end

  test "sets error message on error - tds" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :tds_test_repo])

    try do
      TdsRepo.all(from(u in "users", select: u.non_existent_field))
    rescue
      _ -> :ok
    end

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      status: {:status, :error, message},
                      attributes: attributes
                    )}

    assert message =~ "Invalid column name 'non_existent_field'"

    assert %{unquote(ErrorAttributes.error_type()) => 207} =
             :otel_attributes.map(attributes)
  end

  test "sets error message on error - sqlite3" do
    setup_instrumentation(event_prefix: [:opentelemetry_ecto, :sqlite3_test_repo])

    try do
      Sqlite3Repo.all(from(u in "users", select: u.non_existent_field))
    rescue
      _ -> :ok
    end

    assert_receive {:span,
                    span(
                      name: "SELECT users",
                      status: {:status, :error, message},
                      attributes: attributes
                    )}

    assert message =~ "no such column: u0.non_existent_field"

    assert %{unquote(ErrorAttributes.error_type()) => :_OTHER} =
             :otel_attributes.map(attributes)
  end

  test "preloads in sequence are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    setup_instrumentation()

    Tracer.with_span "parent span" do
      Repo.all(Query.from(User, preload: [:posts, :comments]), in_parallel: false)
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT users"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT comments"
                    )}
  end

  test "preloads in parallel are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    setup_instrumentation()

    Tracer.with_span "parent span" do
      Repo.all(Query.from(User, preload: [:posts, :comments]))
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT users"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT comments"
                    )}
  end

  test "nested query preloads are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    setup_instrumentation()

    Tracer.with_span "parent span" do
      users_query = from(u in User, preload: [:posts, :comments])
      comments_query = from(c in Comment, preload: [user: ^users_query])
      Repo.all(Query.from(User, preload: [:posts, comments: ^comments_query]))
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}
    # root query
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT users"
                    )}

    # comments preload
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT comments"
                    )}

    # users preload
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT users"
                    )}

    # preloads of user
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "SELECT comments"
                    )}
  end

  test "nested query within Task does not skip parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    setup_instrumentation()

    Tracer.with_span "root span" do
      task =
        Task.async(fn ->
          Tracer.with_span "parent span" do
            Repo.all(User)
          end
        end)

      Task.await(task)
    end

    assert_receive {:span, span(span_id: _root_span_id, name: "root span")}
    assert_receive {:span, span(span_id: parent_span_id, name: "parent span")}

    assert_receive {:span,
                    span(
                      parent_span_id: ^parent_span_id,
                      name: "SELECT users"
                    )}
  end
end
