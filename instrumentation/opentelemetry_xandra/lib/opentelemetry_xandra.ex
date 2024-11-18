defmodule OpentelemetryXandra do
  @moduledoc """
  A module to trace Xandra queries with OpenTelemetry.

  This library uses [Telemetry](https://github.com/beam-telemetry/telemetry) to
  create OpenTelemetry Spans for Xandra queries.

  ## Usage

  See `attach/1`.

  ## Resources

  This library follows the OpenTelemetry Semantic Conventions for naming, according to:

    * [The Cassandra conventions](https://opentelemetry.io/docs/specs/semconv/database/cassandra/)
    * [The DB conventions](https://opentelemetry.io/docs/specs/semconv/database/database-spans/)

  """

  @tracer_id __MODULE__

  @typedoc """
  Thet type for a function that returns the statement to be used in the span.

  See `attach/1` for more information.
  """
  @type statement_fun :: (String.t() -> {:ok, String.t()} | :error)

  @typedoc """
  The type for a function that parses a query and returns the operation, database, and table.

  See `attach/1` for more information.
  """
  @type operation_parser_fun ::
          (String.t() -> {operation :: String.t(), database :: String.t(), table :: String.t()})

  @doc """
  Attaches a Telemetry handler that records OTel spans for Xandra queries.

  ## Usage

  Call this function in your application's `c:Application.start/2` callback:

      def start(_type, _args) do
        children = [
          # ...
        ]

        OpentelemetryXandra.setup()

        Supervisor.start_link(children, strategy: :one_for_one)
      end

  ## Options

    * `:operation_parser` - a function that takes a query (as a string) and should
      return a DB operation string that will be used in the span name. For example,
      for a query like `INSERT INTO users (id, name) VALUES (1, 'Alice')`, the
      operation parser could return `INSERT`. The default operation parser
      just takes the first word of the (whitespace-trimmed) query.

    * `:statement` - it can be a boolean, where `true` means that the `db.statement`
      span attribute gets filled with the query statement. If `false`, the attribute
      doesn't get set. It can also be a function of type `t:statement_fun/0`: if it
      returns `{:ok, statement}` then `db.statement` gets set to `statement`, while
      if it returns `:error` then `db.statement` doesn't get set.

  > #### Sensitive Information {: .error}
  >
  > Xandra does not sanitize the query that this library captures. Whatever string
  > you pass to `Xandra.execute/4` and other functions gets used for the `:statement`
  > option.

  """
  @spec setup(keyword()) :: :ok | {:error, :already_exists}
  def setup(options \\ []) when is_list(options) do
    config = %{
      operation_parser: Keyword.get(options, :operation_parser, &parse_operation/1)
    }

    :telemetry.attach_many(
      __MODULE__,
      [
        [:xandra, :execute_query, :start],
        [:xandra, :execute_query, :stop],
        [:xandra, :execute_query, :exception]
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event([:xandra, :execute_query, event], _measurements, metadata, config) do
    _ = handle_event(event, metadata, config)
    :ok
  end

  defp handle_event(:start, metadata, config) do
    attributes = attributes_from_query(metadata.query, config)

    OpentelemetryTelemetry.start_telemetry_span(
      @tracer_id,
      Map.fetch!(attributes, :"db.operation"),
      metadata,
      %{
        kind: :client,
        # TODO: use semantic conventions once the library gets updated to the latest spec.
        attributes:
          Map.merge(attributes, %{
            "db.system": "cassandra",
            "server.address": metadata.address,
            "network.peer.address": metadata.address,
            "network.peer.port": metadata.port
          })
      }
    )
  end

  defp handle_event(:stop, metadata, _config) do
    span_ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    status =
      case Map.get(metadata, :reason) do
        nil -> OpenTelemetry.status(:ok)
        error when is_exception(error) -> OpenTelemetry.status(:error, Exception.message(error))
        other -> OpenTelemetry.status(:error, inspect(other))
      end

    OpenTelemetry.Span.set_status(span_ctx, status)

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp handle_event(:exception, metadata, _config) do
    span_ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, metadata)

    status = OpenTelemetry.status(:error, inspect(metadata.reason))
    OpenTelemetry.Span.set_status(span_ctx, status)

    :otel_span.record_exception(span_ctx, metadata.kind, metadata.reason, metadata.stacktrace, [])

    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp attributes_from_query(query, config)
       when is_struct(query, Xandra.Simple) or is_struct(query, Xandra.Prepared) do
    case config.operation_parser.(query.statement) do
      {operation, database, table} ->
        ["db.operation": operation, "db.name": database, "db.sql.table": table]
        |> Enum.reject(&match?({_, nil}, &1))
        |> Map.new()

      other ->
        raise ArgumentError,
              ":operation_parser must return a tuple with 3 elements, got: #{inspect(other)}"
    end
  end

  defp attributes_from_query(_meta, _config) do
    %{"db.operation": "UNKNOWN"}
  end

  defp parse_operation(statement) do
    case String.trim(statement) do
      "SELECT" <> _rest -> {"SELECT", nil, nil}
      "UPDATE" <> _rest -> {"UPDATE", nil, nil}
      _other -> {"UNKNOWN", nil, nil}
    end
  end
end
