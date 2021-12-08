defmodule OpentelemetryObanTest do
  use DataCase

  doctest OpentelemetryOban

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)

    TestHelpers.remove_oban_handlers()
    OpentelemetryOban.setup()

    :ok
  end

  test "records span on job insertion" do
    OpentelemetryOban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "TestJob send",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :producer,
                      status: :undefined
                    )}

    assert [
             "messaging.destination": "events",
             "messaging.destination_kind": "queue",
             "messaging.oban.job_id": _job_id,
             "messaging.oban.max_attempts": 1,
             "messaging.oban.priority": 0,
             "messaging.oban.worker": "TestJob",
             "messaging.system": "oban"
           ] = List.keysort(attributes, 0)
  end

  test "job creation uses existing trace if present" do
    OpenTelemetry.Tracer.with_span "test span" do
      ctx = OpenTelemetry.Tracer.current_span_ctx()
      root_trace_id = OpenTelemetry.Span.trace_id(ctx)
      root_span_id = OpenTelemetry.Span.span_id(ctx)

      OpentelemetryOban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "TestJob send",
                        attributes: _attributes,
                        trace_id: ^root_trace_id,
                        parent_span_id: ^root_span_id,
                        kind: :producer,
                        status: :undefined
                      )}
    end
  end

  test "keeps existing meta information" do
    OpentelemetryOban.insert(TestJob.new(%{}, meta: %{foo: "bar"}))

    assert [job] = all_enqueued()
    assert job.meta["foo"] == "bar"
  end

  test "tracing information is propagated between send and process" do
    OpentelemetryOban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "TestJob send",
                      attributes: _attributes,
                      trace_id: send_trace_id,
                      span_id: send_span_id,
                      kind: :producer,
                      status: :undefined
                    )}

    assert_receive {:span,
                    span(
                      name: "TestJob process",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: process_trace_id,
                      links: [link(trace_id: ^send_trace_id, span_id: ^send_span_id)]
                    )}

    # Process is ran asynchronously so we create a new trace, but still link
    # the traces together.
    assert send_trace_id != process_trace_id
  end

  test "no link is created on process when tracing info was not propagated" do
    # Using regular Oban, instead of OpentelemetryOban
    Oban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "TestJob process",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: _trace_id,
                      links: []
                    )}
  end

  test "records spans for successful Oban jobs" do
    OpentelemetryOban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "TestJob process",
                      attributes: attributes,
                      kind: :consumer,
                      status: :undefined
                    )}

    assert [
             "messaging.destination": "events",
             "messaging.destination_kind": "queue",
             "messaging.oban.attempt": 1,
             "messaging.oban.inserted_at": _inserted_at,
             "messaging.oban.job_id": _job_id,
             "messaging.oban.max_attempts": 1,
             "messaging.oban.priority": 0,
             "messaging.oban.scheduled_at": _scheduled_at,
             "messaging.oban.worker": "TestJob",
             "messaging.operation": "process",
             "messaging.system": "oban"
           ] = List.keysort(attributes, 0)
  end

  test "records spans for Oban jobs that stop with {:error, :something}" do
    OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}))
    assert %{success: 0, failure: 1} = Oban.drain_queue(queue: :events)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "TestJobThatReturnsError process",
                      attributes: attributes,
                      kind: :consumer,
                      events: [
                        event(
                          name: "exception",
                          attributes: [
                            {"exception.type", "Elixir.Oban.PerformError"},
                            {"exception.message",
                             "TestJobThatReturnsError failed with {:error, :something}"},
                            {"exception.stacktrace", _stacktrace}
                          ]
                        )
                      ],
                      status: ^expected_status
                    )}

    assert [
             "messaging.destination": "events",
             "messaging.destination_kind": "queue",
             "messaging.oban.attempt": 1,
             "messaging.oban.inserted_at": _inserted_at,
             "messaging.oban.job_id": _job_id,
             "messaging.oban.max_attempts": 1,
             "messaging.oban.priority": 0,
             "messaging.oban.scheduled_at": _scheduled_at,
             "messaging.oban.worker": "TestJobThatReturnsError",
             "messaging.operation": "process",
             "messaging.system": "oban"
           ] = List.keysort(attributes, 0)
  end

  test "records spans for each retry" do
    OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}, max_attempts: 2))

    assert %{success: 0, failure: 2} =
             Oban.drain_queue(queue: :events, with_scheduled: true, with_recursion: true)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "TestJobThatReturnsError send",
                      trace_id: send_trace_id,
                      span_id: send_span_id
                    )}

    assert_receive {:span,
                    span(
                      name: "TestJobThatReturnsError process",
                      status: ^expected_status,
                      trace_id: first_process_trace_id,
                      links: [link(trace_id: ^send_trace_id, span_id: ^send_span_id)]
                    )}

    assert_receive {:span,
                    span(
                      name: "TestJobThatReturnsError process",
                      status: ^expected_status,
                      trace_id: second_process_trace_id,
                      links: [link(trace_id: ^send_trace_id, span_id: ^send_span_id)]
                    )}

    assert first_process_trace_id != second_process_trace_id
  end

  test "records spans for Oban jobs that stop with an exception" do
    OpentelemetryOban.insert(TestJobThatThrowsException.new(%{}))
    assert %{success: 0, failure: 1} = Oban.drain_queue(queue: :events)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "TestJobThatThrowsException process",
                      attributes: attributes,
                      kind: :consumer,
                      events: [
                        event(
                          name: "exception",
                          attributes: [
                            {"exception.type", "Elixir.UndefinedFunctionError"},
                            {"exception.message",
                             "function Some.error/0 is undefined (module Some is not available)"},
                            {"exception.stacktrace", _stacktrace}
                          ]
                        )
                      ],
                      status: ^expected_status
                    )}

    assert [
             "messaging.destination": "events",
             "messaging.destination_kind": "queue",
             "messaging.oban.attempt": 1,
             "messaging.oban.inserted_at": _inserted_at,
             "messaging.oban.job_id": _job_id,
             "messaging.oban.max_attempts": 1,
             "messaging.oban.priority": 0,
             "messaging.oban.scheduled_at": _scheduled_at,
             "messaging.oban.worker": "TestJobThatThrowsException",
             "messaging.operation": "process",
             "messaging.system": "oban"
           ] = List.keysort(attributes, 0)
  end

  test "spans inside the job are associated with the job trace" do
    OpentelemetryOban.insert(TestJobWithInnerSpan.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "TestJobWithInnerSpan process",
                      kind: :consumer,
                      trace_id: trace_id,
                      span_id: process_span_id
                    )}

    assert_receive {:span,
                    span(
                      name: "span inside the job",
                      kind: :internal,
                      trace_id: ^trace_id,
                      parent_span_id: ^process_span_id
                    )}
  end

  test "OpentelemetryOban.insert!/2 returns job on successful insert" do
    %Oban.Job{} = OpentelemetryOban.insert!(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)
    assert_receive {:span, span(name: "TestJob send")}
    assert_receive {:span, span(name: "TestJob process")}
  end

  test "OpentelemetryOban.insert!/2 raises an error on failed insert" do
    assert_raise(
      Ecto.InvalidChangesetError,
      fn -> OpentelemetryOban.insert!(TestJob.new(%{}, max_attempts: -1)) end
    )

    assert %{success: 0, failure: 0} = Oban.drain_queue(queue: :events)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "TestJob send",
                      events: [
                        event(
                          name: "exception",
                          attributes: [
                            {"exception.type", "Elixir.Ecto.InvalidChangesetError"},
                            {"exception.message", _message},
                            {"exception.stacktrace", _stacktrace}
                          ]
                        )
                      ],
                      status: ^expected_status
                    )}

    refute_received {:span, span(name: "TestJob process")}
  end

  test "tracing information is propagated when using insert_all/2" do
    OpentelemetryOban.insert_all([
      TestJob.new(%{}),
      TestJob.new(%{})
    ])

    assert %{success: 2, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "Oban bulk insert",
                      attributes: _attributes,
                      trace_id: send_trace_id,
                      span_id: send_span_id,
                      kind: :producer,
                      status: :undefined
                    )}

    assert_receive {:span,
                    span(
                      name: "TestJob process",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: first_process_trace_id,
                      links: [link(trace_id: ^send_trace_id, span_id: ^send_span_id)]
                    )}

    assert_receive {:span,
                    span(
                      name: "TestJob process",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: second_process_trace_id,
                      links: [link(trace_id: ^send_trace_id, span_id: ^send_span_id)]
                    )}

    # Process is ran asynchronously so we create a new trace, but still link
    # the traces together.
    assert send_trace_id != first_process_trace_id
    assert send_trace_id != second_process_trace_id
    assert first_process_trace_id != second_process_trace_id
  end

  test "works with Oban.Testing.perform_job helper function" do
    Oban.Testing.perform_job(TestJob, %{}, repo: TestRepo)

    assert_receive {:span, span(name: "TestJob process")}
  end
end
