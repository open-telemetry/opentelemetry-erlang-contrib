defmodule OpentelemetryOban.JobHandlerTest do
  use DataCase

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span
  require Record

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  defp setup_otel do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1, exporter: {:otel_exporter_pid, self()}}}
    ])

    :application.start(:opentelemetry)
    TestHelpers.remove_oban_handlers()
  end

  describe "span_relationship: :child" do
    setup do
      setup_otel()
      OpentelemetryOban.setup(job: [span_relationship: :child])
      :ok
    end

    test "process span continues the propagated trace as a child of the send span" do
      OpenTelemetry.Tracer.with_span "parent operation" do
        parent_ctx = OpenTelemetry.Tracer.current_span_ctx()
        parent_trace_id = OpenTelemetry.Span.trace_id(parent_ctx)

        OpentelemetryOban.insert(TestJob.new(%{}))
        assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

        assert_receive {:span,
                        span(
                          name: "send events",
                          trace_id: ^parent_trace_id,
                          span_id: send_span_id
                        )}

        assert_receive {:span,
                        span(
                          name: "process events",
                          kind: :consumer,
                          trace_id: ^parent_trace_id,
                          parent_span_id: ^send_span_id,
                          links: links
                        )}

        assert [] == :otel_links.list(links)
      end
    end

    test "process span starts a new trace when no parent context was propagated" do
      Oban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        parent_span_id: :undefined,
                        links: links
                      )}

      assert [] == :otel_links.list(links)
    end

    test "inner spans inside job are children of the process span" do
      OpenTelemetry.Tracer.with_span "parent operation" do
        OpentelemetryOban.insert(TestJobWithInnerSpan.new(%{}))
        assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

        assert_receive {:span,
                        span(
                          name: "process events",
                          kind: :consumer,
                          trace_id: process_trace_id,
                          span_id: process_span_id
                        )}

        assert_receive {:span,
                        span(
                          name: "span inside the job",
                          kind: :internal,
                          trace_id: ^process_trace_id,
                          parent_span_id: ^process_span_id
                        )}
      end
    end

    test "retries share the same propagated trace and are children of the send span" do
      OpenTelemetry.Tracer.with_span "parent operation" do
        parent_ctx = OpenTelemetry.Tracer.current_span_ctx()
        parent_trace_id = OpenTelemetry.Span.trace_id(parent_ctx)

        OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}, max_attempts: 2))

        assert %{success: 0, failure: 1, discard: 1} =
                 Oban.drain_queue(queue: :events, with_scheduled: true, with_recursion: true)

        assert_receive {:span,
                        span(
                          name: "send events",
                          trace_id: ^parent_trace_id,
                          span_id: send_span_id
                        )}

        assert_receive {:span,
                        span(
                          name: "process events",
                          trace_id: first_process_trace_id,
                          parent_span_id: first_parent_span_id
                        )}

        assert_receive {:span,
                        span(
                          name: "process events",
                          trace_id: second_process_trace_id,
                          parent_span_id: second_parent_span_id
                        )}

        assert first_process_trace_id == parent_trace_id
        assert second_process_trace_id == parent_trace_id
        assert first_parent_span_id == send_span_id
        assert second_parent_span_id == send_span_id
      end
    end
  end

  describe "span_relationship: :link (default)" do
    setup do
      setup_otel()
      OpentelemetryOban.setup(job: [span_relationship: :link])
      :ok
    end

    test "process span creates a new trace with a link to the send span" do
      OpentelemetryOban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "send events",
                        trace_id: send_trace_id,
                        span_id: send_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        trace_id: process_trace_id,
                        parent_span_id: :undefined,
                        links: links
                      )}

      [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(links)
      assert send_trace_id != process_trace_id
    end

    test "no link is created when no parent context was propagated" do
      Oban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        parent_span_id: :undefined,
                        links: links
                      )}

      assert [] == :otel_links.list(links)
    end

    test "inner spans inside job are children of the process span" do
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

    test "retries each create a new trace with links" do
      OpentelemetryOban.insert(TestJobThatReturnsError.new(%{}, max_attempts: 2))

      assert %{success: 0, failure: 1, discard: 1} =
               Oban.drain_queue(queue: :events, with_scheduled: true, with_recursion: true)

      assert_receive {:span,
                      span(
                        name: "send events",
                        trace_id: send_trace_id,
                        span_id: send_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "process events",
                        trace_id: first_process_trace_id,
                        links: job_1_links
                      )}

      [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_1_links)

      assert_receive {:span,
                      span(
                        name: "process events",
                        trace_id: second_process_trace_id,
                        links: job_2_links
                      )}

      [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(job_2_links)

      assert first_process_trace_id != second_process_trace_id
    end
  end

  describe "span_relationship: :none" do
    setup do
      setup_otel()
      OpentelemetryOban.setup(job: [span_relationship: :none])
      :ok
    end

    test "process span has no links and no parent context when trace was propagated" do
      OpentelemetryOban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "send events",
                        trace_id: send_trace_id
                      )}

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        trace_id: process_trace_id,
                        parent_span_id: :undefined,
                        links: links
                      )}

      assert [] == :otel_links.list(links)
      assert send_trace_id != process_trace_id
    end

    test "process span has no links when no parent context was propagated" do
      Oban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        parent_span_id: :undefined,
                        links: links
                      )}

      assert [] == :otel_links.list(links)
    end

    test "inner spans inside job are children of the process span" do
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
  end

  describe "default span_relationship" do
    setup do
      setup_otel()
      OpentelemetryOban.setup()
      :ok
    end

    test "defaults to :link behavior" do
      OpentelemetryOban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      assert_receive {:span,
                      span(
                        name: "send events",
                        trace_id: send_trace_id,
                        span_id: send_span_id
                      )}

      assert_receive {:span,
                      span(
                        name: "process events",
                        kind: :consumer,
                        trace_id: process_trace_id,
                        parent_span_id: :undefined,
                        links: links
                      )}

      [link(trace_id: ^send_trace_id, span_id: ^send_span_id)] = :otel_links.list(links)
      assert send_trace_id != process_trace_id
    end
  end

  describe "job: :disabled" do
    setup do
      setup_otel()
      OpentelemetryOban.setup(job: :disabled)
      :ok
    end

    test "does not create process spans when job handler is disabled" do
      Oban.insert(TestJob.new(%{}))
      assert %{success: 1, failure: 0} = Oban.drain_queue(queue: :events)

      refute_receive {:span, span(name: "process events")}, 100
    end
  end
end
