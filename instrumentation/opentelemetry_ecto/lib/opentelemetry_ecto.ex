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
    * `:additional_attributes` - additional attributes to include in the span. If there
      are conflits with default provided attributes, the ones provided with
      this config will have precedence.
    * `:db_statement` - :disabled (default) | :enabled | fun
      Whether or not to include db statements.
      Optionally provide a function that takes a query string and returns a
      sanitized version of it. This is useful for removing sensitive information from the
      query string. Unless this option is `:enabled` or a function,
      query statements will not be recorded on spans.
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

    span_prefix =
      case Keyword.fetch(config, :span_prefix) do
        {:ok, prefix} -> prefix
        :error -> Enum.join(event, ".")
      end

    span_suffix = if source != nil, do: ":#{source}", else: ""
    span_name = span_prefix <> span_suffix

    time_unit = Keyword.get(config, :time_unit, :microsecond)
    additional_attributes = Keyword.get(config, :additional_attributes, %{})

    db_type =
      case type do
        :ecto_sql_query -> :sql
        _ -> type
      end

    # TODO: need connection information to complete the required attributes
    # net.peer.name or net.peer.ip and net.peer.port
    base_attributes = %{
      "db.type": db_type,
      source: source,
      "db.instance": database,
      "db.name": database,
      "db.url": url,
      "total_time_#{time_unit}s": System.convert_time_unit(total_time, :native, time_unit)
    }

    db_statement_config = Keyword.get(config, :db_statement, :disabled)

    attributes =
      base_attributes
      |> add_measurements(measurements, time_unit)
      |> maybe_add_db_statement(db_statement_config, query)
      |> maybe_add_db_system(repo.__adapter__())
      |> add_additional_attributes(additional_attributes)

    parent_context =
      case OpentelemetryProcessPropagator.fetch_ctx(self()) do
        :undefined ->
          OpentelemetryProcessPropagator.fetch_parent_ctx(1, :"$callers")

        ctx ->
          ctx
      end

    parent_token =
      if parent_context != :undefined do
        OpenTelemetry.Ctx.attach(parent_context)
      else
        :undefined
      end

    s =
      OpenTelemetry.Tracer.start_span(span_name, %{
        start_time: start_time,
        attributes: attributes,
        kind: :client
      })

    case query_result do
      {:error, error} ->
        OpenTelemetry.Span.set_status(s, OpenTelemetry.status(:error, format_error(error)))

      {:ok, _} ->
        :ok
    end

    OpenTelemetry.Span.end_span(s)

    if parent_token != :undefined do
      OpenTelemetry.Ctx.detach(parent_token)
    end
  end

  defp format_error(%{__exception__: true} = exception) do
    Exception.message(exception)
  end

  defp format_error(_), do: ""

  defp add_measurements(attributes, measurements, time_unit) do
    measurements
    |> Enum.reduce(attributes, fn
      {k, v}, acc
      when not is_nil(v) and k in [:decode_time, :query_time, :queue_time, :idle_time] ->
        Map.put(
          acc,
          String.to_atom("#{k}_#{time_unit}s"),
          System.convert_time_unit(v, :native, time_unit)
        )

      _, acc ->
        acc
    end)
  end

  defp maybe_add_db_statement(attributes, :enabled, query) do
    Map.put(attributes, :"db.statement", query)
  end

  defp maybe_add_db_statement(attributes, :disabled, _query) do
    attributes
  end

  defp maybe_add_db_statement(attributes, sanitizer, query) when is_function(sanitizer, 1) do
    Map.put(attributes, :"db.statement", sanitizer.(query))
  end

  defp maybe_add_db_statement(attributes, _, _query) do
    attributes
  end

  defp maybe_add_db_system(attributes, Ecto.Adapters.Postgres) do
    Map.put(attributes, :"db.system", :postgresql)
  end

  defp maybe_add_db_system(attributes, Ecto.Adapters.MyXQL) do
    Map.put(attributes, :"db.system", :mysql)
  end

  defp maybe_add_db_system(attributes, Ecto.Adapters.SQLite3) do
    Map.put(attributes, :"db.system", :sqlite)
  end

  defp maybe_add_db_system(attributes, Ecto.Adapters.Tds) do
    Map.put(attributes, :"db.system", :mssql)
  end

  defp maybe_add_db_system(attributes, _) do
    attributes
  end

  defp add_additional_attributes(attributes, additional_attributes) do
    Map.merge(attributes, additional_attributes)
  end
end
