defmodule BasicElixir.Worker do
  use GenServer
  require Logger
  require OpenTelemetry.Tracer, as: Tracer
  require OpenTelemetry.Span

  # Client
  def start_link(default) when is_list(default) do
    # Span names can be a binary or an atom. Atoms are preferred
    # for memory efficiency.
    Tracer.with_span :start_link do
      # Span name rules also apply to events
      Tracer.add_event("Nice operation!", %{"bogons" => 100})

      # Attributes can be a map or a keyword list
      Tracer.set_attributes(%{another_key: "yes"})

      # Any attributes that can be known before starting a span
      # should be passed on span creation so that they can be
      # available for sampling decisions
      Tracer.with_span "Sub operation...", %{attributes: [lemons_key: "five"]} do
        Tracer.add_event("Sub span event!", [])
      end

      GenServer.start_link(__MODULE__, default)
    end
  end

  def push(pid, element) do
    GenServer.cast(pid, {:push, element})
  end

  def pop(pid) do
    GenServer.call(pid, :pop)
  end

  # Server (callbacks)
  @impl true
  def init(stack) do
    Tracer.with_span :init do
      Logger.info("Starting #{__MODULE__}...")
      {:ok, stack}
    end
  end

  @impl true
  def handle_call(:pop, _from, [head | tail]) do
    {:reply, head, tail}
  end

  @impl true
  def handle_cast({:push, element}, state) do
    {:noreply, [element | state]}
  end
end
