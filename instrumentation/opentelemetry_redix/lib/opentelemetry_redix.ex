defmodule OpentelemetryRedix do
  @moduledoc """
  OpentelemetryRedix uses [telemetry](https://hexdocs.pm/telemetry/) handlers to
  create `OpenTelemetry` spans.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryRedix.setup()

        # ...
      end

  """

  alias OpenTelemetry.SemanticConventions.Trace
  alias OpentelemetryRedix.Command
  alias OpentelemetryRedix.ConnectionTracker

  require Trace
  require OpenTelemetry.Tracer

  @typedoc "Setup options"
  @type opts :: []

  @doc """
  Initializes and configures the telemetry handlers.

  You may also supply the following options:

    * `db_statement` - `:enabled`, `:disabled` or a single arity function. If
      `:enabled`, the DB statement is sanitized of sensitive values, such as
      AUTH crendentials, but otherwise logged. If `:disabled`, the statement is
      not logged. If you pass in a single arity function, the list of commands
      will be passed through that function for your own processing. Defaults to
      `:enabled`.
  """
  @spec setup(opts()) :: :ok
  def setup(config \\ []) do
    :telemetry.attach(
      {__MODULE__, :pipeline_stop},
      [:redix, :pipeline, :stop],
      &__MODULE__.handle_pipeline_stop/4,
      config
    )
  end

  @doc false
  def handle_pipeline_stop(_event, measurements, meta, config) do
    add_db_statement = Keyword.get(config, :db_statement, :enabled)

    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    operation =
      case meta.commands do
        [[operation | _args]] -> operation
        _pipeline -> "pipeline"
      end

    connection = ConnectionTracker.get_connection(meta.connection)

    attributes =
      %{
        Trace.db_system() => "redis",
        Trace.db_operation() => operation
      }
      |> Map.merge(db_statement_attributes(meta, add_db_statement))
      |> Map.merge(net_attributes(connection))
      |> Map.merge(redix_attributes(meta))

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
      OpenTelemetry.Tracer.start_span(operation, %{
        start_time: start_time,
        kind: :client,
        attributes: attributes
      })

    if meta[:kind] == :error do
      OpenTelemetry.Span.set_status(s, OpenTelemetry.status(:error, format_error(meta.reason)))
    end

    OpenTelemetry.Span.end_span(s)

    if parent_token != :undefined do
      OpenTelemetry.Ctx.detach(parent_token)
    end
  end

  defp net_attributes(%{address: address}) when is_binary(address) do
    [host, port] = address |> String.split(":")
    %{Trace.net_peer_name() => host, Trace.net_peer_port() => port}
  end

  defp net_attributes(_), do: %{}

  defp redix_attributes(%{connection_name: nil}), do: %{}
  defp redix_attributes(%{connection_name: name}), do: %{"db.redix.connection_name": name}
  defp redix_attributes(_), do: %{}

  defp db_statement_attributes(meta, :enabled) do
    db_statement = Enum.map_join(meta.commands, "\n", &Command.sanitize/1)
    %{Trace.db_statement() => db_statement}
  end

  defp db_statement_attributes(_meta, :disabled), do: %{}

  defp db_statement_attributes(meta, fun) when is_function(fun, 1) do
    db_statement = fun.(meta.commands)
    %{Trace.db_statement() => db_statement}
  end

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)
end
