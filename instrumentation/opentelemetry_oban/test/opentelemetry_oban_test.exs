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
    {:ok, %{id: id}} = OpentelemetryOban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "send events",
                      attributes: attributes,
                      parent_span_id: :undefined,
                      kind: :producer,
                      status: :undefined
                    )}

    assert %{
             "messaging.destination.name": "events",
             "messaging.operation.name": "send",
             "oban.job.job_id": _job_id,
             "oban.job.max_attempts": 1,
             "oban.job.priority": 0,
             "oban.job.worker": "TestJob",
             "messaging.message.id": ^id,
             "messaging.client.id": "TestJob",
             "messaging.operation.type": :create,
             "messaging.system": :oban
           } = :otel_attributes.map(attributes)
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
                        name: "send events",
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
                      name: "send events",
                      attributes: _attributes,
                      trace_id: send_trace_id,
                      span_id: send_span_id,
                      kind: :producer,
                      status: :undefined
                    )}

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: process_trace_id,
                      links: links
                    )}

    [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(links)

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
                      name: "process events",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: _trace_id,
                      links: links
                    )}

    assert [] == :otel_links.list(links)
  end

  test "records spans for successful Oban jobs" do
    {:ok, %{id: id}} = OpentelemetryOban.insert(TestJob.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: attributes,
                      kind: :consumer,
                      status: :undefined
                    )}

    assert %{
             "messaging.destination.name": "events",
             "messaging.operation.name": "process",
             "oban.job.attempt": 1,
             "oban.job.inserted_at": _inserted_at,
             "oban.job.job_id": ^id,
             "oban.job.max_attempts": 1,
             "oban.job.priority": 0,
             "oban.job.scheduled_at": _scheduled_at,
             "oban.job.worker": "TestJob",
             "messaging.message.id": ^id,
             "messaging.client.id": "TestJob",
             "messaging.operation.type": :process,
             "messaging.system": :oban
           } = :otel_attributes.map(attributes)
  end

  test "records spans for Oban jobs that stop with {:error, :something}" do
    {:ok, %{id: id}} = OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}))
    assert %{success: 0, discard: 1} = Oban.drain_queue(queue: :events)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: attributes,
                      kind: :consumer,
                      events: events,
                      status: ^expected_status
                    )}

    assert %{
             "oban.job.attempt": 1,
             "oban.job.inserted_at": _inserted_at,
             "oban.job.job_id": ^id,
             "oban.job.max_attempts": 1,
             "oban.job.priority": 0,
             "oban.job.scheduled_at": _scheduled_at,
             "oban.job.worker": "TestJobThatReturnsError",
             "messaging.destination.name": "events",
             "messaging.operation.name": "process",
             "messaging.operation.type": :process,
             "messaging.system": :oban,
             "messaging.message.id": ^id,
             "messaging.client.id": "TestJobThatReturnsError"
           } = :otel_attributes.map(attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "records spans for each retry" do
    OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}, max_attempts: 2))

    assert %{success: 0, failure: 1, discard: 1} =
             Oban.drain_queue(queue: :events, with_scheduled: true, with_recursion: true)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "send events",
                      trace_id: send_trace_id,
                      span_id: send_span_id
                    )}

    assert_receive {:span,
                    span(
                      name: "process events",
                      status: ^expected_status,
                      trace_id: first_process_trace_id,
                      links: job_1_links
                    )}

    [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_1_links)

    assert_receive {:span,
                    span(
                      name: "process events",
                      status: ^expected_status,
                      trace_id: second_process_trace_id,
                      links: job_2_links
                    )}

    [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_2_links)

    assert first_process_trace_id != second_process_trace_id
  end

  test "records spans for Oban jobs that stop with an exception" do
    {:ok, %{id: id}} = OpentelemetryOban.insert(TestJobThatThrowsException.new(%{}))
    assert %{success: 0, discard: 1} = Oban.drain_queue(queue: :events)

    expected_status = OpenTelemetry.status(:error, "")

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: attributes,
                      kind: :consumer,
                      events: events,
                      status: ^expected_status
                    )}

    assert %{
             "messaging.destination.name": "events",
             "messaging.operation.name": "process",
             "oban.job.attempt": 1,
             "oban.job.inserted_at": _inserted_at,
             "oban.job.job_id": ^id,
             "oban.job.max_attempts": 1,
             "oban.job.priority": 0,
             "oban.job.scheduled_at": _scheduled_at,
             "oban.job.worker": "TestJobThatThrowsException",
             "messaging.operation.type": :process,
             "messaging.message.id": ^id,
             "messaging.client.id": "TestJobThatThrowsException",
             "messaging.system": :oban
           } = :otel_attributes.map(attributes)

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))
  end

  test "spans inside the job are associated with the job trace" do
    OpentelemetryOban.insert(TestJobWithInnerSpan.new(%{}))
    assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "process events",
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
    assert_receive {:span, span(name: "send events")}
    assert_receive {:span, span(name: "process events")}
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
                      name: "send events",
                      events: events,
                      status: ^expected_status
                    )}

    [
      event(
        name: :exception,
        attributes: event_attributes
      )
    ] = :otel_events.list(events)

    assert [:"exception.message", :"exception.stacktrace", :"exception.type"] ==
             Enum.sort(Map.keys(:otel_attributes.map(event_attributes)))

    refute_received {:span, span(name: "process events")}
  end

  test "tracing information is propagated when using insert_all/2" do
    OpentelemetryOban.insert_all([
      TestJob.new(%{}),
      TestJob.new(%{})
    ])

    assert %{success: 2, failure: 0} = Oban.drain_queue(queue: :events)

    assert_receive {:span,
                    span(
                      name: "send",
                      attributes: _attributes,
                      trace_id: send_trace_id,
                      span_id: send_span_id,
                      kind: :producer,
                      status: :undefined
                    )}

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: first_process_trace_id,
                      links: job_1_links
                    )}

    [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_1_links)

    assert_receive {:span,
                    span(
                      name: "process events",
                      attributes: _attributes,
                      kind: :consumer,
                      status: :undefined,
                      trace_id: second_process_trace_id,
                      links: job_2_links
                    )}

    [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_2_links)

    # Process is ran asynchronously so we create a new trace, but still link
    # the traces together.
    assert send_trace_id != first_process_trace_id
    assert send_trace_id != second_process_trace_id
    assert first_process_trace_id != second_process_trace_id
  end

  test "works with Oban.Testing.perform_job helper function" do
    Oban.Testing.perform_job(TestJob, %{}, repo: TestRepo)

    assert_receive {:span, span(name: "process events")}
  end
end
