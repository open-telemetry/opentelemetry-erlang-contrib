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
  """
  @spec setup(opts()) :: :ok
  def setup(_opts \\ []) do
    :telemetry.attach(
      {__MODULE__, :pipeline_stop},
      [:redix, :pipeline, :stop],
      &__MODULE__.handle_pipeline_stop/4,
      :no_config
    )
  end

  @doc false
  def handle_pipeline_stop(_event, measurements, meta, _config) do
    duration = measurements.duration
    end_time = :opentelemetry.timestamp()
    start_time = end_time - duration

    operation =
      case meta.commands do
        [[operation | _args]] -> operation
        _pipeline -> "pipeline"
      end

    statement = Enum.map_join(meta.commands, "\n", &Command.sanitize/1)

    connection = ConnectionTracker.get_connection(meta.connection)

    attributes =
      %{
        Trace.db_system() => "redis",
        Trace.db_operation() => operation,
        Trace.db_statement() => statement
      }
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

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(reason), do: inspect(reason)
end
