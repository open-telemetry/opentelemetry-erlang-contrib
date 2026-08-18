defmodule OpentelemetryOban.PluginIntegrationTest do
  @moduledoc """
  Exercises real Oban plugins so the attributes and statuses are asserted against the metadata
  Oban actually emits, rather than against hand-written `:telemetry.execute/3` payloads.
  """

  use DataCase

  require Record

  alias Oban.Job

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry/include/otel_span.hrl") do
    Record.defrecord(name, spec)
  end

  for {name, spec} <- Record.extract_all(from_lib: "opentelemetry_api/include/opentelemetry.hrl") do
    Record.defrecord(name, spec)
  end

  defmodule RaisingPlugin do
    @moduledoc false

    @behaviour Oban.Plugin

    use GenServer

    @impl Oban.Plugin
    def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: opts[:name])

    @impl Oban.Plugin
    def validate(_opts), do: :ok

    @impl GenServer
    def init(opts), do: {:ok, %{conf: opts[:conf]}}

    @impl GenServer
    def handle_info(:run, state) do
      meta = %{conf: state.conf, plugin: __MODULE__}

      :telemetry.span([:oban, :plugin], meta, fn ->
        raise ArgumentError, "boom from plugin"
      end)

      {:noreply, state}
    end
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

  test "Oban.Plugins.Lifeline reports the jobs it actually rescued and discarded" do
    orphaned_at = DateTime.add(DateTime.utc_now(), -2, :hour)

    insert_orphan(attempt: 1, max_attempts: 20, attempted_at: orphaned_at)
    insert_orphan(attempt: 20, max_attempts: 20, attempted_at: orphaned_at)

    run_plugin({Oban.Plugins.Lifeline, interval: :timer.hours(1), rescue_after: 1}, :rescue)

    assert %{
             "oban.plugin": "Oban.Plugins.Lifeline",
             "oban.plugins.lifeline.rescued_count": 1,
             "oban.plugins.lifeline.discarded_count": 1
           } = receive_span_attrs(Oban.Plugins.Lifeline)
  end

  test "a failing plugin produces an errored span" do
    # REINDEX CONCURRENTLY cannot run inside the sandbox transaction, so the plugin fails for real
    # and reports it through the {:error, meta} return value of :telemetry.span/3.
    run_plugin({Oban.Plugins.Reindexer, schedule: "@daily"}, :reindex)

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Reindexer process",
                      status: status(code: :error)
                    )},
                   1000
  end

  test "a non-leader plugin run is not reported as an error" do
    run_plugin({Oban.Plugins.Reindexer, schedule: "@daily"}, :reindex,
      peer: {Oban.Peers.Isolated, leader?: false}
    )

    assert_receive {:span,
                    span(
                      name: "Oban.Plugins.Reindexer process",
                      attributes: attributes,
                      status: :undefined
                    )},
                   1000

    refute Map.has_key?(:otel_attributes.map(attributes), :"error.type")
  end

  test "a raising plugin records the exception message" do
    run_plugin(RaisingPlugin, :run)

    expected_name = "#{inspect(RaisingPlugin)} process"

    assert_receive {:span,
                    span(
                      name: ^expected_name,
                      attributes: attributes,
                      events: events
                    )},
                   1000

    assert %{"error.type": "ArgumentError"} = :otel_attributes.map(attributes)

    [event(name: :exception, attributes: event_attributes)] = :otel_events.list(events)

    assert %{
             "exception.type": "Elixir.ArgumentError",
             "exception.message": "boom from plugin"
           } = :otel_attributes.map(event_attributes)
  end

  defp insert_orphan(fields) do
    TestRepo.insert!(
      struct(%Job{args: %{}, queue: "events", state: "executing", worker: "TestJob"}, fields)
    )
  end

  defp run_plugin(plugin, message, opts \\ []) do
    peer = Keyword.get(opts, :peer, Oban.Peers.Isolated)
    plugin_module = if is_tuple(plugin), do: elem(plugin, 0), else: plugin

    start_supervised!(
      {Oban,
       name: __MODULE__.Oban,
       repo: TestRepo,
       notifier: Oban.Notifiers.PG,
       peer: peer,
       queues: [],
       stage_interval: :infinity,
       plugins: [plugin]}
    )

    __MODULE__.Oban
    |> Oban.Registry.whereis({:plugin, plugin_module})
    |> send(message)
  end

  defp receive_span_attrs(plugin) do
    name = "#{inspect(plugin)} process"

    assert_receive(
      {:span, span(name: ^name, attributes: attributes)},
      1000,
      "expected span with name #{name} to be received"
    )

    :otel_attributes.map(attributes)
  end
end
