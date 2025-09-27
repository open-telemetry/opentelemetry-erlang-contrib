defmodule OpentelemetryEcto do
  require OpenTelemetry.Tracer

  alias OpenTelemetry.SemConv.ErrorAttributes
  alias OpenTelemetry.SemConv.Incubating.DBAttributes
  alias OpenTelemetry.SemConv.ServerAttributes

  alias OpentelemetryEcto.EctoAttributes
  # how to handle db.query.parameter.<key>?
  # not parseable from current meta
  opt_outs = [
    DBAttributes.db_query_text()
  ]

  @options_schema NimbleOptions.new!(
                    event_prefix: [
                      type: {:list, :atom},
                      required: true,
                      type_spec: quote(do: :telemetry.event_name()),
                      doc: """
                      Must be the prefix configured in the `Ecto.Repo` Telemetry configuration.

                      By default, it's the camel_case name of the repository module. For `MyApp.Repo`, it would
                      be `[:my_app, :repo]`.
                      """
                    ],
                    opt_out_attrs: [
                      type: {:list, {:in, opt_outs}},
                      default: [],
                      type_spec: quote(do: opt_out_attrs()),
                      doc: """
                      List of attributes with a `Requirement Level` of `Recommended` to opt out of.

                      By default, instrumentation libraries implement all `Recommended` attributes in addition
                      to `Required` attributes.

                      Use semantic conventions library to ensure compatability, e.g. `[DBAttributes.db_query_text()]`

                      Recommended Attributes:
                      #{Enum.map_join(opt_outs, "\n\n", &"  * `#{inspect(&1)}`")}
                      """
                    ],
                    telemetry_metadata_preprocessor: [
                      type: {:fun, 1},
                      default: &__MODULE__.default_metadata_preprocessor/1,
                      doc: """
                      Preprocessor for the telemetry metadata used for instrumentation.

                      This can be used to sanitize adapter-specific metadata such as
                      a query or error's text.

                      No keys may be deleted from the map, only modified. Refer to the
                      [Ecto Repo Telemetry Information](`m:Ecto.Repo#module-adapter-specific-events`) page
                      for more information.
                      """
                    ],
                    additional_span_attributes: [
                      type: :map,
                      type_spec: quote(do: OpenTelemetry.attributes_map()),
                      doc: """
                      Additional attributes to include on all spans. Instrumented
                      attributes take precedence over anything supplied here.
                      """,
                      default: %{}
                    ],
                    repo_metadata_storage: [
                      type: {:in, [:ets, :persistent_term]},
                      default: :persistent_term,
                      type_spec: quote(do: :ets | :persistent_term),
                      doc: """
                      Storage mechanism for repo metadata used for attributes.

                      This should only be set to `:ets` when very large numbers
                      of dynamic repositories are used. Additionally, this setting
                      may only be set once with subsequent settings being ignored.
                      """,
                      type_doc: ":atom"
                    ]
                  )

  @moduledoc """
  OpenTelemetry instrumentation for Ecto.

  ## Setup

  This should be called from your application's `c:Application.start/2` callback on startup,
  before starting the application's top-level supervisor.

  ## Semantic Conventions

  All required and recommended DB Client Call Span semantic conventions are implemented.
  Supported opt-in attributes can be configured using the `opt_in_attrs` option.

  > #### Note {: .info}
  >
  > DB attribute Semantic Conventions are still mostly experimental and subject
  > to change.

  ### Opt-out Attributes

  By default, instrumentation libraries implement all `Recommended` attributes in addition
  to `Required` attributes.

  See [SemConv DB Span Common Attributes](`e:opentelemetry_semantic_conventions:database-spans.md#common-attributes`) for attribute requirement levels.

  ### Query and Error Sanitization

  Ecto SQL queries are parameterized for all major adapters, requiring no further sanitization.
  In some rare cases, you may still need further processing.

  Some exceptions emitted may contain sensitive information in the exception message and
  are not aware of `redacted` field settings in Ecto schemas. In these cases, you may
  wish to sanitize these error messages.

  For these use cases, `telemetry_metadata_preprocessor` may be utilized to preprocess these
  fields.

  ### Per-Query Options

  Additional attributes can be provided and a span name override on a per-query basis. This can be
  useful for providing more tailored information where the instrumentation cannot provide such.
  For instance, when calling a stored procedure, the span name could be updated with the name
  of the procedure.

  Note that attributes can only be additive. Existing attributes will not be overridden. In
  several cases, semantic attributes cannot be reliably set. Where these are not set by
  this instrumentation, you may still set those attribtutes.

  ```
  Repo.all(User,
    telemetry_options: [
      otel: %{
        span_name: "custom span name",
        attributes: %{
          "config.attribute": "special value overwritten",
          "db.system": "my_system",
          extra: "should add"
        }
      }
    ]
  )
  ```

  ## Preloads

  > #### Note {: .neutral}
  >
  > Due to limitations with how Ecto emits its telemetry, nested preloads are not
  > represented as nested spans within a trace.

  Any relation preloads, which are executed in parallel in separate tasks, will be
  linked to the span of the process that initiated the call. For example:

      Tracer.with_span "parent span" do
        Repo.all(Query.from(User, preload: [:posts, :comments]))
      end

  This will create a span called `"parent span"` with three child spans for each
  query: users, posts, and comments.

  ## Dynamic Repositories, Replicas, and Telemetry Prefixes

  To accurately report host connection information, OpentelemetryEcto uses information
  contained in the `[:ecto, :repo, :init]` telemetry event since the

  > #### Warning {: .warning}
  >
  > Telemetry prefixes should not be reused between repos to ensure connection information
  > is accurate. Subsequent `:init` events for a given prefix will override previously
  > seen connection info.

  """

  @typedoc "Use semantic conventions library to ensure compatability, e.g. `DBAttributes.db_query_text()`"
  @type opt_out_attr() ::
          unquote(DBAttributes.db_query_text())

  @type opt_out_attrs() :: [opt_out_attr()]

  @type options() :: [unquote(NimbleOptions.option_typespec(@options_schema))]

  @doc """
  Initializes and configures the telemetry handlers.

  Supported options:\n#{NimbleOptions.docs(@options_schema)}
  """
  @spec setup(options()) :: :ok | {:error, :already_exists}
  def setup(opts) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})

    event = config.event_prefix ++ [:query]

    attach_init_handler(config)

    attach_query_handler(event, config)
  end

  defp attach_init_handler(config) do
    :telemetry.attach(
      {__MODULE__, :init, config.event_prefix},
      [:ecto, :repo, :init],
      &__MODULE__.handle_init/4,
      config
    )
  end

  defp attach_query_handler(event, config) do
    result = :telemetry.attach({__MODULE__, event}, event, &__MODULE__.handle_event/4, config)

    case result do
      :ok ->
        :ok

      {:error, :already_exists} ->
        :error
    end
  end

  defp get_meta_table do
    case :ets.whereis(:otel_ecto_repo_meta) do
      :undefined ->
        :ets.new(:otel_ecto_repo_meta, [:public, :named_table, :set, {:read_concurrency, true}])
        |> :ets.whereis()

      tid ->
        tid
    end
  end

  defp get_repo_metadata(key, :persistent_term) do
    config = :persistent_term.get(:otel_ecto, %{repo_meta: %{}})
    Map.get(config.repo_meta, key, %{})
  end

  defp get_repo_metadata(key, :ets) do
    case :ets.lookup(:otel_ecto_repo_meta, key) do
      [] -> %{}
      [meta] -> meta
    end
  end

  defp query_opts(%{options: options}) do
    case Keyword.get(options, :otel) do
      nil -> %{}
      opts when is_map(opts) -> opts
      _ -> %{}
    end
  end

  defp query_opts(_), do: %{}

  @doc false
  def handle_init([:ecto, :repo, :init], %{}, meta, config) do
    key = {meta.repo, meta.opts[:telemetry_prefix] || config.event_prefix}

    if config.repo_metadata_storage == :persistent_term do
      term = :persistent_term.get(:otel_ecto, %{repo_meta: %{}})
      updated_term = %{repo_meta: Map.put(term.repo_meta, key, Map.new(meta.opts))}
      :persistent_term.put(:otel_ecto, updated_term)
    else
      tid = get_meta_table()

      :ets.insert(tid, {key, Map.new(meta.opts)})
    end
  end

  @doc false
  def handle_event(_event, measurements, meta, config) do
    %{query: query, source: source, result: query_result, repo: repo, type: type} =
      config.telemetry_metadata_preprocessor.(meta)

    per_query_opts = query_opts(meta)

    # Doing all this even if the span isn't sampled so the sampler
    # could technically use the attributes to decide if it should sample or not

    total_time = measurements.total_time
    end_time = :opentelemetry.timestamp()
    start_time = end_time - total_time

    repo_config =
      get_repo_metadata({repo, config.event_prefix}, config.repo_metadata_storage)

    attributes =
      %{
        unquote(DBAttributes.db_system()) => db_system(repo.__adapter__()),
        unquote(DBAttributes.db_namespace()) => repo_config.database,
        unquote(DBAttributes.db_query_text()) => query
      }
      |> set_db_query_text(query, type)
      |> set_db_collection_name(source)
      |> set_server_address(repo, repo_config)
      |> maybe_add_server_port(repo.__adapter__(), repo_config)
      |> set_db_operation_name(query, type)
      |> maybe_add_error_type(repo.__adapter__(), query_result)
      |> add_measurements(measurements)
      |> set_additional_attributes(config, per_query_opts)

    span_name = span_name(attributes, per_query_opts)

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

  @doc false
  def default_metadata_preprocessor(meta), do: meta

  defp format_error(%{__exception__: true} = exception) do
    Exception.message(exception)
  end

  defp format_error(_), do: ""

  defp set_server_address(attrs, repo, repo_config) do
    case repo.__adapter__() do
      Ecto.Adapters.SQLite3 ->
        Map.put(attrs, ServerAttributes.server_address(), repo_config.database)

      _ ->
        Map.put(attrs, ServerAttributes.server_address(), repo_config.hostname)
    end
  end

  db_systems = [
    {Ecto.Adapters.Postgres, DBAttributes.db_system_values().postgresql},
    {Ecto.Adapters.MyXQL, DBAttributes.db_system_values().mysql},
    {Ecto.Adapters.SQLite3, DBAttributes.db_system_values().sqlite},
    {Ecto.Adapters.Tds, DBAttributes.db_system_values().mssql}
  ]

  for {adapter, system} <- db_systems do
    defp db_system(unquote(adapter)), do: unquote(system)
  end

  # NOTE: This is the catch-all clause where we use other_sql as the db.system value, but it may not be a SQL based database.
  defp db_system(_), do: DBAttributes.db_system_values().other_sql

  defp set_db_collection_name(attributes, source) do
    Map.put(attributes, DBAttributes.db_collection_name(), source)
  end

  # only set port for non-standard ports
  defp maybe_add_server_port(attributes, adapter, repo_config) do
    case {adapter, Map.get(repo_config, :port)} do
      {_, nil} -> attributes
      {Ecto.Adapters.Postgres, 5432} -> attributes
      {Ecto.Adapters.MyXQL, 3306} -> attributes
      {Ecto.Adapters.Tds, 1433} -> attributes
      {Ecto.Adapters.SQLite3, _} -> attributes
      {_, port} -> Map.put(attributes, ServerAttributes.server_port(), port)
    end
  end

  defp set_db_operation_name(attributes, query, :ecto_sql_query) do
    Map.put(attributes, unquote(DBAttributes.db_operation_name()), extract_sql_command(query))
  end

  defp set_db_operation_name(attributes, _, _), do: attributes

  defp extract_sql_command("SELECT " <> _rest), do: :SELECT
  defp extract_sql_command("INSERT " <> _rest), do: :INSERT
  defp extract_sql_command("UPDATE " <> _rest), do: :UPDATE
  defp extract_sql_command("DELETE " <> _rest), do: :DELETE
  defp extract_sql_command("WITH " <> _rest), do: :WITH
  defp extract_sql_command("BEGIN " <> _rest), do: :BEGIN
  defp extract_sql_command("COMMIT " <> _rest), do: :COMMIT
  defp extract_sql_command("ROLLBACK " <> _rest), do: :ROLLBACK
  defp extract_sql_command("CREATE " <> _rest), do: :CREATE
  defp extract_sql_command("ALTER " <> _rest), do: :ALTER
  defp extract_sql_command("DROP " <> _rest), do: :DROP
  defp extract_sql_command("TRUNCATE " <> _rest), do: :TRUNCATE
  defp extract_sql_command("USE " <> _rest), do: :USE
  defp extract_sql_command("SHOW " <> _rest), do: :SHOW
  defp extract_sql_command("DESCRIBE " <> _rest), do: :DESCRIBE
  defp extract_sql_command("EXPLAIN " <> _rest), do: :EXPLAIN
  defp extract_sql_command("SET " <> _rest), do: :SET
  defp extract_sql_command("GRANT " <> _rest), do: :GRANT
  defp extract_sql_command("REVOKE " <> _rest), do: :REVOKE
  defp extract_sql_command("SAVEPOINT " <> _rest), do: :SAVEPOINT
  defp extract_sql_command("RELEASE " <> _rest), do: :RELEASE
  defp extract_sql_command("PREPARE " <> _rest), do: :PREPARE
  defp extract_sql_command("EXECUTE " <> _rest), do: :EXECUTE
  defp extract_sql_command("DEALLOCATE " <> _rest), do: :DEALLOCATE
  defp extract_sql_command("CALL " <> _rest), do: :CALL
  defp extract_sql_command("FETCH " <> _rest), do: :FETCH
  defp extract_sql_command("DECLARE " <> _rest), do: :DECLARE
  defp extract_sql_command("CLOSE " <> _rest), do: :CLOSE
  defp extract_sql_command("DISCARD " <> _rest), do: :DISCARD
  defp extract_sql_command("LISTEN " <> _rest), do: :LISTEN
  defp extract_sql_command("NOTIFY " <> _rest), do: :NOTIFY
  defp extract_sql_command("REINDEX " <> _rest), do: :REINDEX
  defp extract_sql_command("VACUUM " <> _rest), do: :VACUUM
  defp extract_sql_command("CLUSTER " <> _rest), do: :CLUSTER
  defp extract_sql_command("COPY " <> _rest), do: :COPY
  defp extract_sql_command("ANALYZE " <> _rest), do: :ANALYZE

  # Fallback clause for unrecognized commands
  defp extract_sql_command(_query), do: :UNKNOWN

  defp maybe_add_error_type(attributes, _adapter, {:ok, _}), do: attributes

  defp maybe_add_error_type(attributes, adapter, {:error, error}) do
    error_type = get_error_type(adapter, error)
    Map.put(attributes, unquote(ErrorAttributes.error_type()), error_type)
  end

  defp get_error_type(Ecto.Adapters.Postgres, %{postgres: %{code: code}}), do: code
  defp get_error_type(Ecto.Adapters.MyXQL, %{mysql: %{code: code}}), do: code
  defp get_error_type(Ecto.Adapters.Tds, %{mssql: %{number: number}}), do: number
  defp get_error_type(Ecto.Adapters.SQLite3, _), do: :_OTHER
  defp get_error_type(_adapter, _), do: :_OTHER

  defp add_measurements(attributes, measurements) do
    measurements
    |> Enum.reduce(attributes, fn
      {k, v}, attrs when not is_nil(v) ->
        set_measurement(
          attrs,
          k,
          System.convert_time_unit(v, :native, :nanosecond) / 1_000_000_000
        )

      _, attrs ->
        attrs
    end)
  end

  defp set_measurement(attrs, :total_time, time) do
    Map.put(attrs, unquote(EctoAttributes.ecto_total_time_duration()), time)
  end

  defp set_measurement(attrs, :decode_time, time) do
    Map.put(attrs, unquote(EctoAttributes.ecto_decode_time_duration()), time)
  end

  defp set_measurement(attrs, :query_time, time) do
    Map.put(attrs, unquote(EctoAttributes.ecto_query_time_duration()), time)
  end

  defp set_measurement(attrs, :queue_time, time) do
    Map.put(attrs, unquote(EctoAttributes.ecto_queue_time_duration()), time)
  end

  defp set_measurement(attrs, :idle_time, time) do
    Map.put(attrs, unquote(EctoAttributes.ecto_idle_time_duration()), time)
  end

  defp set_measurement(attrs, _measurement, _time), do: attrs

  defp set_db_query_text(attributes, query, :ecto_sql_query) do
    Map.put(attributes, unquote(DBAttributes.db_query_text()), query)
  end

  defp set_db_query_text(attributes, _, _), do: attributes

  defp set_additional_attributes(attrs, %{additional_span_attributes: extra}, %{
         attributes: per_query_attrs
       })
       when is_map(per_query_attrs) do
    extra
    |> Map.merge(per_query_attrs)
    |> Map.merge(attrs)
  end

  defp set_additional_attributes(attrs, %{additional_span_attributes: extra}, _) do
    Map.merge(extra, attrs)
  end

  defp set_additional_attributes(attrs, _, _), do: attrs

  defp span_name(_, %{span_name: name}) when is_atom(name) or is_binary(name), do: name

  # SHOULD be `{db.operation.name} {target}` if there is a (low-cardinality) {db.operation.name} available.
  defp span_name(
         %{unquote(DBAttributes.db_operation_name()) => db_operation_name} = attributes,
         _
       ),
       do: "#{db_operation_name} #{target(attributes)}"

  # If there is no (low-cardinality) `db.operation.name` available, database span names SHOULD be `{target}`.
  defp span_name(attributes, _), do: target(attributes)

  # `db.collection.name` SHOULD be used for data manipulation operations or operations on database collections.
  defp target(%{unquote(DBAttributes.db_collection_name()) => db_collection_name}),
    do: db_collection_name

  # `db.namespace` SHOULD be used for operations on a specific database namespace.
  defp target(%{unquote(DBAttributes.db_namespace()) => db_namespace}), do: db_namespace
end
