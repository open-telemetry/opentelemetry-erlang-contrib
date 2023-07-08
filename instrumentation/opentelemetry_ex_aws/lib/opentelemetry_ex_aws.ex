defmodule OpentelemetryExAws do
  @moduledoc """
  OpentelemetryExAws uses `telemetry` handlers to create `OpenTelemetry` spans from ExAws events.

  ## Usage

  In your application start:

      def start(_type, _args) do
        OpentelemetryExAws.setup()

        # ...
      end

  You may also provide a custom prefix, if you have configured ExAws to use one:

      def start(_type, _args) do
        OpentelemetryExAws.setup([:ex_aws, :custom_prefix])

        # ...
      end

  """
  alias OpenTelemetry.SemanticConventions.Trace
  alias OpenTelemetryExAws.DynamoDB

  require Trace
  require OpenTelemetry.Tracer

  @tracer_id __MODULE__

  def setup(prefix \\ [:ex_aws, :request]) do
    :telemetry.attach(
      {__MODULE__, :handle_request_start},
      prefix ++ [:start],
      &__MODULE__.handle_request_start/4,
      %{}
    )

    :telemetry.attach(
      {__MODULE__, :handle_request_stop},
      prefix ++ [:stop],
      &__MODULE__.handle_request_stop/4,
      %{}
    )
  end

  def handle_request_start(_event, _measurements, metadata, _config) do
    operation = Map.fetch!(metadata, :operation)
    [service, method] = String.split(operation, ".", parts: 2)

    attributes = %{
      Trace.rpc_method() => method,
      Trace.rpc_service() => service,
      Trace.rpc_system() => "aws-api"
    }

    OpentelemetryTelemetry.start_telemetry_span(@tracer_id, operation, metadata, %{
      attributes: attributes,
      kind: :client
    })
  end

  def handle_request_stop(_event, _measurements, metadata, _config) do
    attributes = get_attributes(metadata)
    OpenTelemetry.Tracer.set_attributes(attributes)
    OpentelemetryTelemetry.end_telemetry_span(@tracer_id, metadata)
  end

  defp get_attributes(%{operation: "DynamoDB" <> _rest} = metadata), do: DynamoDB.get_attributes(metadata)
end
