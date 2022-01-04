defmodule OpentelemetryRedix.ConnectionTracker do
  @moduledoc false
  use GenServer

  @conn_table __MODULE__.ETS

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def get_connection(tab \\ @conn_table, pid) do
    case :ets.lookup(tab, pid) do
      [{_pid, metadata}] ->
        metadata

      [] ->
        nil
    end
  end

  @impl true
  def init(opts) do
    config = %{
      tab: Keyword.get(opts, :ets_table, @conn_table)
    }

    :ets.new(config.tab, [:named_table, :public, read_concurrency: true])

    :telemetry.attach(
      {__MODULE__, :connection},
      [:redix, :connection],
      &__MODULE__.handle_connection/4,
      config
    )

    :telemetry.attach(
      {__MODULE__, :disconnection},
      [:redix, :disconnection],
      &__MODULE__.handle_disconnection/4,
      config
    )

    Process.flag(:trap_exit, true)
    {:ok, config}
  end

  @impl true
  def terminate(_reason, _state) do
    :telemetry.detach({__MODULE__, :connection})
    :telemetry.detach({__MODULE__, :disconnection})
  end

  def handle_connection(_event, _measurements, meta, config) do
    connection_attrs = Map.take(meta, [:address, :connection_name, :reconnection])

    :ets.insert_new(config.tab, {meta.connection, connection_attrs})
  end

  def handle_disconnection(_event, _measurements, meta, config) do
    :ets.delete(config.tab, meta.connection)
  end
end
