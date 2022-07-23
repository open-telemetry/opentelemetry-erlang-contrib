defmodule OpentelemetryEcto do
  @moduledoc """
  Telemetry handler for creating OpenTelemetry Spans from Ecto query events. Any
  relation preloads, which are executed in parallel in separate tasks, will be
  linked to the span of the process that initiated the call. For example:

      Tracer.with_span "parent span" do
        Repo.all(Query.from(User, preload: [:posts, :comments]))
      end

  this will create a span called "parent span" with three child spans for each
  query: users, posts, and comments.

  > #### Note {: .neutral}
  >
  > Due to limitations with how Ecto emits its telemetry, nested preloads are not
  > represented as nested spans within a trace.
  """

  require OpenTelemetry.Tracer

  @doc """
  Attaches the OpentelemetryEcto handler to your repo events. This should be called
  from your application behaviour on startup.

  Example:

      OpentelemetryEcto.setup([:blog, :repo])

  You may also supply the following options in the second argument:

    * `:time_unit` - a time unit used to convert the values of query phase
      timings, defaults to `:microsecond`. See `System.convert_time_unit/3`

    * `:span_prefix` - the first part of the span name, as a `String.t`,
      defaults to the concatenation of the event name with periods, e.g.
      `"blog.repo.query"`. This will always be followed with a colon and the
      source (the table name for SQL adapters).
  """
  def setup(event_prefix, config \\ []) do
    event = event_prefix ++ [:query]
    :telemetry.attach({__MODULE__, event}, event, &__MODULE__.handle_event/4, config)
  end

  @doc false
  def handle_event(
        event,
        measurements,
        %{query: query, source: source, result: query_result, repo: repo, type: type},
        config
      ) do
    # Doing all this even if the span isn't sampled so the sampler
    # could technically use the attributes to decide if it should sample or not
    total_time = measurements.total_time
    end_time = :opentelemetry.timestamp()
    start_time = end_time - total_time

    %{
      hostname: hostname,
      username: username,
      database: database,
      port: port
    } =
      [hostname: nil, username: nil, database: nil, port: nil]
      |> Keyword.merge(repo.config())
      |> Keyword.merge(Ecto.Repo.Supervisor.parse_url(repo.config()[:url] || ""))
      |> Keyword.take([:hostname, :username, :database, :port])
      |> Enum.into(%{})

    url =
      "ecto://#{hostname}:#{port}/#{database}"
      |> URI.parse()
      |> URI.to_string()

    span_name =
      case Keyword.fetch(config, :span_prefix) do
        {:ok, prefix} -> prefix
        :error -> Enum.join(event, ".")
      end
      |> maybe_append_source(source)

    time_unit = Keyword.get(config, :time_unit, :microsecond)

    base_attributes =
      %{
        "db.type": if(type == :ecto_sql_query, do: :sql, else: type),
        "db.statement": query,
        "db.system": db_system(repo.__adapter__()),
        "db.instance": database,
        "db.connection_string": url,
        "db.user": username,
        "db.name": database,
        "net.peer.name": hostname,
        "net.transport": "IP.TCP",
        "total_time_#{time_unit}s": System.convert_time_unit(total_time, :native, time_unit)
      }
      |> maybe_add(:"db.sql.table", source)
      |> maybe_add(:"net.peer.port", port)
      |> maybe_add_peer_addr(hostname)

    attributes =
      measurements
      |> Stream.take_while(fn {k, _} -> k in [:decode_time, :query_time, :queue_time, :idle_time] end)
      |> Stream.map(fn {k, v} -> {:"#{k}_#{time_unit}s", System.convert_time_unit(v, :native, time_unit)} end)
      |> Enum.into(%{})

    parent_context = OpentelemetryProcessPropagator.fetch_parent_ctx(1, :"$callers")

    if ctx?(parent_context), do: OpenTelemetry.Ctx.attach(parent_context)

    s =
      OpenTelemetry.Tracer.start_span(span_name, %{
        start_time: start_time,
        attributes: Map.merge(attributes, base_attributes),
        kind: :client
      })

    case query_result do
      {:error, error} ->
        OpenTelemetry.Span.set_status(s, OpenTelemetry.status(:error, format_error(error)))

      {:ok, _} ->
        :ok
    end

    OpenTelemetry.Span.end_span(s)

    if ctx?(parent_context), do: OpenTelemetry.Ctx.detach(parent_context)
  end

  defp format_error(%{__exception__: true} = exception) do
    Exception.message(exception)
  end

  defp format_error(_), do: ""

  defp ctx?(:undefined), do: false
  defp ctx?(_), do: true

  defp maybe_add(map, _, nil), do: map
  defp maybe_add(map, key, value) when is_map(map), do: Map.put(map, key, value)

  defp maybe_append_source(name, nil), do: name
  defp maybe_append_source(name, source), do: "#{name}:#{source}"

  defp maybe_add_peer_addr(map, peer_name) do
    if ip?(peer_name), do: maybe_add(map, :"net.sock.peer.addr", peer_name), else: map
  end

  defp db_system(Ecto.Adapters.Postgres), do: "postgresql"
  defp db_system(Ecto.Adapters.MyXQL), do: "mysql"
  defp db_system(Ecto.Adapters.Tds), do: "mssql"
  defp db_system(other) when is_atom(other), do: other |> Atom.to_string() |> String.downcase()
  defp db_system(other) when is_binary(other), do: other |> String.downcase()
  defp db_system(_), do: "unknown"

  defp ip?(address) when is_binary(address), do: address |> to_charlist() |> ip?()

  defp ip?(address) when is_list(address) do
    case :inet.parse_address(address) do
      {:ok, _} -> true
      _ -> false
    end
  end

  defp ip?(_), do: false
end
