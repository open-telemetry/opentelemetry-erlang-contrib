defmodule OpentelemetryEcto do
  @moduledoc """
  Telemetry handler for creating OpenTelemetry Spans from Ecto query events.

  Any relation preloads, which are executed in parallel in separate tasks, will be
  linked to the span of the process that initiated the call. For example:

      Tracer.with_span "parent span" do
        Repo.all(Query.from(User, preload: [:posts, :comments]))
      end

  This will create a span called `"parent span:"` with three child spans for each
  query: users, posts, and comments.

  > #### Note {: .neutral}
  >
  > Due to limitations with how Ecto emits its telemetry, nested preloads are not
  > represented as nested spans within a trace.
  """

  require OpenTelemetry.Tracer

  alias OpenTelemetry.SemConv.Incubating.{DBAttributes, Metrics.DBMetrics}
  alias OpenTelemetry.SemConv.{ErrorAttributes, ServerAttributes}

  @typedoc """
  Option that you can pass to `setup/2`.
  """
  @typedoc since: "1.3.0"
  @type setup_option() ::
          {:additional_attributes, %{String.t() => term()}}
          | {:db_query, :enabled | :disabled | (String.t() -> String.t())}

  @doc """
  Attaches the `OpentelemetryEcto` handler to your repo events.

  This should be called from your application's `c:Application.start/2` callback on startup,
  before starting the application's top-level supervisor.

  `event_prefix` must be the prefix configured in the `Ecto.Repo` Telemetry configuration.
  By default, it's the camel_case name of the repository module. For `MyApp.Repo`, it would
  be `[:my_app, :repo]`.

  For example:

      @impl Application
      def start(_type, _args) do
        OpentelemetryEcto.setup([:blog, :repo])

        children = [...]
        Supervisor.start_link(children, strategy: :one_for_one)
      end

  ## Options

  You may also supply the following options in the second argument:

    * `:additional_attributes` - additional attributes to include in the span. If there
      are conflicts with default provided attributes, the ones provided with
      this config will have precedence.
    * `:db_query` - `:disabled` (default), `:enabled`, or a function.
      Whether or not to include DB statements in the **span attributes** (as the
      `#{DBAttributes.db_query_text()}` attribute).
      Optionally provide a function that takes a query string and returns a
      sanitized version of it. This is useful for removing sensitive information from the
      query string. Unless this option is `:enabled` or a function,
      query statements will not be recorded on spans.

  """
  @spec setup(:telemetry.event_name(), [setup_option()]) :: :ok | {:error, :already_exists}
  def setup(event_prefix, options \\ []) when is_list(options) do
    event = event_prefix ++ [:query]
    :telemetry.attach({__MODULE__, event}, event, &__MODULE__.handle_event/4, options)
  end

  @doc false
  def handle_event(
        _event,
        measurements,
        %{query: query, source: source, result: query_result, repo: repo},
        config
      ) do
    # Doing all this even if the span isn't sampled so the sampler
    # could technically use the attributes to decide if it should sample or not

    total_time = measurements.total_time
    end_time = :opentelemetry.timestamp()
    start_time = end_time - total_time
    measurements = Map.put(measurements, :total_time, total_time)
    repo_config = Keyword.take(repo.config(), [:database, :hostname, :port])

    additional_attributes = Keyword.get(config, :additional_attributes, %{})

    db_statement_config = Keyword.get(config, :db_query, :disabled)

    # TODO: need connection information to complete the required attributes
    # net.peer.name or net.peer.ip and net.peer.port
    attributes =
      %{
        unquote(DBAttributes.db_system()) => db_system(repo.__adapter__()),
        unquote(DBAttributes.db_namespace()) => repo_config[:database],
        unquote(ServerAttributes.server_address()) => repo_config[:hostname]
      }
      |> maybe_add_db_collection_name(source)
      |> maybe_add_server_port(repo_config)
      |> maybe_add_db_operation_name(repo.__adapter__(), query)
      |> maybe_add_error_type(repo.__adapter__(), query_result)
      |> maybe_add_db_query_text(db_statement_config, query)
      |> add_measurements(measurements)
      |> add_additional_attributes(additional_attributes)

    span_name = span_name(attributes)

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

  @db_systems [
    {Ecto.Adapters.Postgres, DBAttributes.db_system_values().postgresql},
    {Ecto.Adapters.MyXQL, DBAttributes.db_system_values().mysql},
    {Ecto.Adapters.SQLite3, DBAttributes.db_system_values().sqlite},
    {Ecto.Adapters.Tds, DBAttributes.db_system_values().mssql}
  ]

  for {adapter, system} <- @db_systems do
    defp db_system(unquote(adapter)), do: unquote(system)
  end

  # NOTE: This is the catch-all clause where we use other_sql as the db.system value, but it may not be a SQL based database.
  defp db_system(_), do: unquote(DBAttributes.db_system_values().other_sql)

  defp maybe_add_db_collection_name(attributes, nil), do: attributes

  defp maybe_add_db_collection_name(attributes, source) do
    Map.put(attributes, unquote(DBAttributes.db_collection_name()), source)
  end

  defp maybe_add_server_port(attributes, repo_config) do
    case Keyword.has_key?(repo_config, :port) do
      false -> attributes
      true -> Map.put(attributes, unquote(ServerAttributes.server_port()), repo_config[:port])
    end
  end

  defp maybe_add_db_operation_name(attributes, adapter, query) do
    case get_db_operation_name(adapter, query) do
      nil -> attributes
      operation_name -> Map.put(attributes, unquote(DBAttributes.db_operation_name()), operation_name)
    end
  end

  # NOTE: Postgres does set a `:command` attribute on the result, but since there is no command for the
  # error struct we will parse it all the same here.
  defp get_db_operation_name(Ecto.Adapters.Postgres, query), do: sql_command(query)
  defp get_db_operation_name(Ecto.Adapters.MyXQL, query), do: sql_command(query)
  defp get_db_operation_name(Ecto.Adapters.SQLite3, query), do: sql_command(query)
  defp get_db_operation_name(Ecto.Adapters.Tds, query), do: sql_command(query)
  defp get_db_operation_name(_, _), do: nil

  defp sql_command(query) when is_binary(query) do
    query
    |> String.split(" ", trim: true)
    |> sql_command()
  end

  @sql_commands ~w(select insert update delete begin commit)

  defp sql_command([raw_command | _rest]) do
    case String.downcase(raw_command) do
      command when command in @sql_commands -> raw_command
      _ -> nil
    end
  end

  defp maybe_add_error_type(attributes, _adapter, {:ok, _}), do: attributes

  defp maybe_add_error_type(attributes, adapter, {:error, error}) do
    case get_error_type(adapter, error) do
      nil -> attributes
      error_type -> Map.put(attributes, unquote(ErrorAttributes.error_type()), error_type)
    end
  end

  defp get_error_type(Ecto.Adapters.Postgres, %{postgres: %{code: code}}), do: code
  defp get_error_type(Ecto.Adapters.MyXQL, %{postgres: %{name: name}}), do: name
  # NOTE: Exqlite.Error does not have an error type
  # TODO: Normalize error type from the error message?
  defp get_error_type(Ecto.Adapters.SQLite3, _), do: nil
  defp get_error_type(Ecto.Adapters.Tds, %{mssql: %{number: number}}), do: number
  defp get_error_type(_adapter, _), do: nil

  @measurements [
    idle_time: DBMetrics.db_client_connection_create_time(),
    total_time: DBMetrics.db_client_operation_duration(),
    queue_time: DBMetrics.db_client_connection_wait_time(),
    query_time: DBMetrics.db_client_connection_use_time()
  ]

  defp add_measurements(attributes, measurements) do
    Enum.reduce(@measurements, attributes, fn {telemetry_key, attribute_key}, attributes ->
      case Map.get(measurements, telemetry_key) do
        nil -> attributes
        value -> Map.put(attributes, attribute_key, System.convert_time_unit(value, :native, :microsecond) / 1_000_000)
      end
    end)
  end

  defp maybe_add_db_query_text(attributes, :enabled, query) do
    Map.put(attributes, unquote(DBAttributes.db_query_text()), query)
  end

  defp maybe_add_db_query_text(attributes, :disabled, _query) do
    attributes
  end

  defp maybe_add_db_query_text(attributes, sanitizer, query) when is_function(sanitizer, 1) do
    Map.put(attributes, unquote(DBAttributes.db_query_text()), sanitizer.(query))
  end

  defp maybe_add_db_query_text(attributes, _, _query) do
    attributes
  end

  defp add_additional_attributes(attributes, additional_attributes) do
    Map.merge(attributes, additional_attributes)
  end

  # SHOULD be `{db.operation.name} {target}` if there is a (low-cardinality) {db.operation.name} available.
  defp span_name(%{unquote(DBAttributes.db_operation_name()) => db_operation_name} = attributes),
    do: "#{db_operation_name} #{target(attributes)}"

  # If there is no (low-cardinality) `db.operation.name` available, database span names SHOULD be `{target}`.
  defp span_name(attributes), do: target(attributes)

  # `db.collection.name` SHOULD be used for data manipulation operations or operations on database collections.
  defp target(%{unquote(DBAttributes.db_collection_name()) => db_collection_name}), do: db_collection_name

  # `db.namespace` SHOULD be used for operations on a specific database namespace.
  defp target(%{unquote(DBAttributes.db_namespace()) => db_namespace}), do: db_namespace
end
