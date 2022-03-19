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

  alias OpentelemetryRedix.Command
  alias OpentelemetryRedix.ConnectionTracker

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
        "db.system": "redis",
        "db.operation": operation,
        "db.statement": statement
      }
      |> Map.merge(net_attributes(connection))
      |> Map.merge(redix_attributes(meta))
      |> Map.merge(error_attributes(meta))

    s =
      OpenTelemetry.Tracer.start_span(operation, %{
        start_time: start_time,
        kind: :client,
        attributes: attributes
      })

    if meta[:reason] do
      OpenTelemetry.Span.set_status(s, OpenTelemetry.status(:error, ""))
    end

    OpenTelemetry.Span.end_span(s)
  end

  defp net_attributes(%{address: address}) when is_binary(address) do
    [host, port] = address |> String.split(":")
    %{"net.peer.name": host, "net.peer.port": port}
  end

  defp net_attributes(_), do: %{}

  defp redix_attributes(%{connection_name: nil}), do: %{}
  defp redix_attributes(%{connection_name: name}), do: %{"db.redix.connection_name": name}
  defp redix_attributes(_), do: %{}

  defp error_attributes(%{reason: reason}), do: %{"db.redix.error": inspect(reason)}
  defp error_attributes(_), do: %{}
end
