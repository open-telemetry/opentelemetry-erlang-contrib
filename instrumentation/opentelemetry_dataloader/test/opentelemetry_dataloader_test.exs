defmodule OpentelemetryDataloaderTest do
  use ExUnit.Case

  require OpenTelemetry.Tracer

  alias OpentelemetryDataloader.TestRepo, as: Repo
  alias OpentelemetryDataloader.TestModels.Post

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
      :telemetry.detach({__MODULE__, :batch})
      :telemetry.detach({__MODULE__, :run})
    end)
  end

  test "captures dataloader ecto source events" do
    OpentelemetryDataloader.setup()

    source = Dataloader.Ecto.new(Repo)

    loader = Dataloader.new() |> Dataloader.add_source(:db, source)

    loader =
      loader
      |> Dataloader.load(:db, Post, 1)
      |> Dataloader.load_many(:db, Post, [4, 9])

    Dataloader.run(loader)

    assert_receive {:span,
                    span(
                      name: "dataloader.run",
                      attributes: attributes,
                      kind: :client
                    )}

    assert %{} = :otel_attributes.map(attributes)

    assert_receive {:span,
                    span(
                      name: "dataloader.batch",
                      attributes: attributes,
                      kind: :client
                    )}

    assert %{"dataloader.batch_key" => key} = :otel_attributes.map(attributes)
    assert key =~ ~r/OpentelemetryDataloader.TestModels.Post/
  end

  test "keeps a context already attached to self instead of a more distant $callers ancestor" do
    test_pid = self()

    ancestor_pid =
      spawn(fn ->
        "ancestor" |> OpenTelemetry.Tracer.start_span() |> OpenTelemetry.Tracer.set_current_span()

        ancestor_trace_id =
          OpenTelemetry.Tracer.current_span_ctx() |> OpenTelemetry.Span.trace_id()

        send(test_pid, {:ancestor_trace_id, ancestor_trace_id})

        receive do
          :stop -> :ok
        end
      end)

    assert_receive {:ancestor_trace_id, ancestor_trace_id}

    Process.put(:"$callers", [ancestor_pid])
    "self" |> OpenTelemetry.Tracer.start_span() |> OpenTelemetry.Tracer.set_current_span()
    self_trace_id = OpenTelemetry.Tracer.current_span_ctx() |> OpenTelemetry.Span.trace_id()

    OpentelemetryDataloader.handle_event(
      [:dataloader, :source, :run, :start],
      %{},
      %{},
      %{tracer_id: OpentelemetryDataloader}
    )

    run_trace_id = OpenTelemetry.Tracer.current_span_ctx() |> OpenTelemetry.Span.trace_id()

    assert run_trace_id == self_trace_id
    refute run_trace_id == ancestor_trace_id

    send(ancestor_pid, :stop)
    OpenTelemetry.Tracer.end_span()
    OpenTelemetry.Tracer.end_span()
  end
end
