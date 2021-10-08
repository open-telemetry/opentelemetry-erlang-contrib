defmodule OpentelemetryEcto do
  @moduledoc """
  Telemetry handler for creating OpenTelemetry Spans from Ecto query events.
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
    database = repo.config()[:database]

    url =
      case repo.config()[:url] do
        nil ->
          # TODO: add port
          URI.to_string(%URI{scheme: "ecto", host: repo.config()[:hostname]})

        url ->
          url
      end

    span_name =
      case Keyword.fetch(config, :span_prefix) do
        {:ok, prefix} -> prefix
        :error -> Enum.join(event, ".")
      end <> ":#{source}"

    time_unit = Keyword.get(config, :time_unit, :microsecond)

    db_type =
      case type do
        :ecto_sql_query -> :sql
        _ -> type
      end

    result =
      case query_result do
        {:ok, _} -> []
        _ -> [error: true]
      end

    # TODO: need connection information to complete the required attributes
    # net.peer.name or net.peer.ip and net.peer.port
    base_attributes =
      Keyword.merge(result,
        "db.type": db_type,
        "db.statement": query,
        source: source,
        "db.instance": database,
        "db.url": url,
        "total_time_#{time_unit}s": System.convert_time_unit(total_time, :native, time_unit)
      )

    attributes =
      measurements
      |> Enum.into(%{})
      |> Map.take(~w(decode_time query_time queue_time)a)
      |> Enum.reject(&is_nil(elem(&1, 1)))
      |> Enum.map(fn {k, v} ->
        {String.to_atom("#{k}_#{time_unit}s"), System.convert_time_unit(v, :native, time_unit)}
      end)

    s =
      OpenTelemetry.Tracer.start_span(span_name, %{
        start_time: start_time,
        attributes: attributes ++ base_attributes
      })

    OpenTelemetry.Span.end_span(s)
  end
end
