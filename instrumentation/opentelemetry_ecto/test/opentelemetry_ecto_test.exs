defmodule OpentelemetryEctoTest do
  use ExUnit.Case
  import Ecto.Query
  require OpenTelemetry.Tracer

  alias OpentelemetryEcto.TestRepo, as: Repo
  alias OpentelemetryEcto.TestModels.{Comment, User, Post}

  require Ecto.Query, as: Query
  require OpenTelemetry.Tracer, as: Tracer

  @event_name [:opentelemetry_ecto, :test_repo]

  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :application.start(:opentelemetry)

    :otel_batch_processor.set_exporter(:otel_exporter_pid, self())

    OpenTelemetry.Tracer.start_span("test")

    on_exit(fn ->
      OpenTelemetry.Tracer.end_span()
    end)
  end

  test "captures basic query events" do
    attach_handler()

    Repo.all(User)

    assert_receive {:span,
                    span(
                      name: "opentelemetry_ecto.test_repo.query:users",
                      attributes: attributes,
                      kind: :client
                    )}

    assert %{
             "db.system": :postgresql,
             "db.instance": "opentelemetry_ecto_test",
             "db.type": :sql,
             "db.url": "ecto://localhost",
             decode_time_microseconds: _,
             query_time_microseconds: _,
             queue_time_microseconds: _,
             source: "users",
             total_time_microseconds: _
           } = :otel_attributes.map(attributes)
  end

  test "exclude unsantized query" do
    attach_handler()
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}
    assert !Map.has_key?(:otel_attributes.map(attributes), :"db.statement")
  end

  test "include unsanitized query when enabled" do
    attach_handler(db_statement: :enabled)
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"db.statement": "SELECT u0.\"id\", u0.\"email\" FROM \"users\" AS u0"} =
             :otel_attributes.map(attributes)
  end

  test "include santized query with sanitizer function" do
    attach_handler(db_statement: fn str -> String.replace(str, "SELECT", "") end)
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"db.statement": " u0.\"id\", u0.\"email\" FROM \"users\" AS u0"} =
             :otel_attributes.map(attributes)
  end

  test "include additional_attributes" do
    attach_handler(additional_attributes: %{"config.attribute": "special value", "db.instance": "my_instance"})

    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"config.attribute": "special value", "db.instance": "my_instance"} =
             :otel_attributes.map(attributes)
  end

  test "changes the time unit" do
    attach_handler(time_unit: :millisecond)

    Repo.all(Post)

    assert_receive {:span,
                    span(
                      name: "opentelemetry_ecto.test_repo.query:posts",
                      attributes: attributes
                    )}

    assert %{
             "db.system": :postgresql,
             "db.instance": "opentelemetry_ecto_test",
             "db.type": :sql,
             "db.url": "ecto://localhost",
             decode_time_milliseconds: _,
             query_time_milliseconds: _,
             queue_time_milliseconds: _,
             source: "posts",
             total_time_milliseconds: _
           } = :otel_attributes.map(attributes)
  end

  test "changes the span name prefix" do
    attach_handler(span_prefix: "Ecto")

    Repo.all(User)

    assert_receive {:span, span(name: "Ecto:users")}
  end

  test "collects multiple spans" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})

    attach_handler()

    User
    |> Repo.all()
    |> Repo.preload([:posts])

    assert_receive {:span, span(name: "opentelemetry_ecto.test_repo.query:users")}
    assert_receive {:span, span(name: "opentelemetry_ecto.test_repo.query:posts")}
  end

  test "sets error message on error" do
    attach_handler()

    try do
      Repo.all(from(u in "users", select: u.non_existant_field))
    rescue
      _ -> :ok
    end

    assert_receive {:span,
                    span(
                      name: "opentelemetry_ecto.test_repo.query:users",
                      status: {:status, :error, message}
                    )}

    assert message =~ "non_existant_field does not exist"
  end

  test "preloads in sequence are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    attach_handler()

    Tracer.with_span "parent span" do
      Repo.all(Query.from(User, preload: [:posts, :comments]), in_parallel: false)
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:users"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:comments"
                    )}
  end

  test "preloads in parallel are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    attach_handler()

    Tracer.with_span "parent span" do
      Repo.all(Query.from(User, preload: [:posts, :comments]))
    end

    assert_receive {:span, span(span_id: root_span_id, name: "parent span")}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:users"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:comments"
                    )}
  end

  test "nested query preloads are tied to the parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    attach_handler()

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
                      name: "opentelemetry_ecto.test_repo.query:users"
                    )}

    # comments preload
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:comments"
                    )}

    # users preload
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:users"
                    )}

    # preloads of user
    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:posts"
                    )}

    assert_receive {:span,
                    span(
                      parent_span_id: ^root_span_id,
                      name: "opentelemetry_ecto.test_repo.query:comments"
                    )}
  end

  test "nested query within Task does not skip parent span" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})
    Repo.insert!(%Comment{body: "We got traced!", user: user})

    attach_handler()

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
                      name: "opentelemetry_ecto.test_repo.query:users"
                    )}
  end

  def attach_handler(config \\ []) do
    # For now setup the handler manually in each test
    handler = {__MODULE__, self()}

    :telemetry.attach(handler, @event_name ++ [:query], &OpentelemetryEcto.handle_event/4, config)

    on_exit(fn ->
      :telemetry.detach(handler)
    end)
  end
end
