defmodule OpentelemetryEctoTest do
  alias OpentelemetryEcto.TestRepo, as: Repo
  alias OpentelemetryEcto.TestModels.{User, Post}
  require OpenTelemetry.Tracer
  use ExUnit.Case

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
                      attributes: list,
                      kind: :client
                    )}

    assert [
             "db.instance": "opentelemetry_ecto_test",
             "db.statement": "SELECT u0.\"id\", u0.\"email\" FROM \"users\" AS u0",
             "db.type": :sql,
             "db.url": "ecto://localhost",
             decode_time_microseconds: _,
             query_time_microseconds: _,
             queue_time_microseconds: _,
             source: "users",
             total_time_microseconds: _
           ] = List.keysort(list, 0)
  end

  test "changes the time unit" do
    attach_handler(time_unit: :millisecond)

    Repo.all(Post)

    assert_receive {:span,
                    span(
                      name: "opentelemetry_ecto.test_repo.query:posts",
                      attributes: list
                    )}

    assert [
             "db.instance": "opentelemetry_ecto_test",
             "db.statement": "SELECT p0.\"id\", p0.\"body\", p0.\"user_id\" FROM \"posts\" AS p0",
             "db.type": :sql,
             "db.url": "ecto://localhost",
             decode_time_milliseconds: _,
             query_time_milliseconds: _,
             queue_time_milliseconds: _,
             source: "posts",
             total_time_milliseconds: _
           ] = List.keysort(list, 0)
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

  def attach_handler(config \\ []) do
    # For now setup the handler manually in each test
    handler = {__MODULE__, self()}

    :telemetry.attach(handler, @event_name ++ [:query], &OpentelemetryEcto.handle_event/4, config)

    on_exit(fn ->
      :telemetry.detach(handler)
    end)
  end
end
