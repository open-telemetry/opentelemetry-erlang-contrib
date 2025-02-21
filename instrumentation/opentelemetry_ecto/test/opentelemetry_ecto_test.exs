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
                      name: "SELECT users",
                      attributes: attributes,
                      kind: :client
                    )}

    attributes = :otel_attributes.map(attributes)

    assert %{
             "db.client.operation.duration": _,
             "db.client.connection.wait_time": _,
             "db.client.connection.use_time": _
           } = attributes

    attributes =
      Map.drop(
        attributes,
        ~w(db.client.operation.duration db.client.connection.wait_time db.client.connection.use_time)a
      )

    assert attributes == %{
             "db.system": :postgresql,
             "db.collection.name": "users",
             "db.namespace": "opentelemetry_ecto_test",
             "server.address": "localhost",
             "db.operation.name": "SELECT"
           }
  end

  test "exclude unsantized query" do
    attach_handler()
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}
    assert !Map.has_key?(:otel_attributes.map(attributes), :"db.query.text")
  end

  test "include unsanitized query when enabled" do
    attach_handler(db_query: :enabled)
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"db.query.text": "SELECT u0.\"id\", u0.\"email\" FROM \"users\" AS u0"} =
             :otel_attributes.map(attributes)
  end

  test "include sanitized query with sanitizer function" do
    attach_handler(db_query: fn str -> String.replace(str, "SELECT", "") end)
    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"db.query.text": " u0.\"id\", u0.\"email\" FROM \"users\" AS u0"} =
             :otel_attributes.map(attributes)
  end

  test "include additional_attributes" do
    attach_handler(additional_attributes: %{"config.attribute": "special value", "db.system": "my_system"})

    Repo.all(User)

    assert_receive {:span, span(attributes: attributes)}

    assert %{"config.attribute": "special value", "db.system": "my_system"} =
             :otel_attributes.map(attributes)
  end

  test "collects multiple spans" do
    user = Repo.insert!(%User{email: "opentelemetry@erlang.org"})
    Repo.insert!(%Post{body: "We got traced!", user: user})

    attach_handler()

    User
    |> Repo.all()
    |> Repo.preload([:posts])

    assert_receive {:span, span(name: "SELECT users")}
    assert_receive {:span, span(name: "SELECT posts")}
  end

  test "sets error message on error" do
    attach_handler()

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

    assert %{"error.type": :undefined_column} = :otel_attributes.map(attributes)
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

    attach_handler()

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
                      name: "SELECT users"
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
