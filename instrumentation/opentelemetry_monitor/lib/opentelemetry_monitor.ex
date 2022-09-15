defmodule OpentelemetryMonitor do
  use GenServer

  def start_link(_arg) do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(nil) do
    _table_id = :ets.new(__MODULE__, [:bag, :public, {:write_concurrency, true}, :named_table])
    {:ok, nil}
  end

  def handle_call({:monitor, pid}, _from, state) do
    Process.monitor(pid)
    {:reply, :ok, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, :normal}, state) do
    :ets.take(__MODULE__, pid)
    |> Enum.each(fn {_pid, ctx} ->
      _span_ctx = OpenTelemetry.Tracer.set_current_span(ctx)
      _ = OpenTelemetry.Tracer.end_span()
    end)

    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, {:shutdown, _}}, state) do
    :ets.take(__MODULE__, pid)
    |> Enum.each(fn {_pid, ctx} ->
      _span_ctx = OpenTelemetry.Tracer.set_current_span(ctx)
      _ = OpenTelemetry.Tracer.end_span()
    end)

    {:noreply, state}
  end

  def handle_info({:DOWN, _ref, :process, pid, reason}, state) do
    :ets.take(__MODULE__, pid)
    |> Enum.each(fn {_pid, ctx} ->
      _span_ctx = OpenTelemetry.Tracer.set_current_span(ctx)
      _ = OpenTelemetry.Tracer.add_event("Process died", [{"reason", inspect(reason)}])
      _ = OpenTelemetry.Tracer.end_span()
    end)

    {:noreply, state}
  end

  def monitor(span_ctx) do
    if Application.fetch_env!(:opentelemetry, :processors) != [] do
      # monitor first, because the monitor is necessary to clean the ets table.
      :ok = GenServer.call(__MODULE__, {:monitor, self()})
      true = :ets.insert(__MODULE__, {self(), span_ctx})
    end
  end
end
