defmodule OpentelemetryAbsinthe.ValidateHandler do
  @moduledoc false

  @validate_start [:absinthe, :validate, :start]
  @validate_stop [:absinthe, :validate, :stop]

  @doc false
  def attach(config) do
    :telemetry.attach_many(
      {__MODULE__, :validate},
      [
        @validate_start,
        @validate_stop
      ],
      &__MODULE__.handle_event/4,
      config
    )
  end

  @doc false
  def handle_event(event, measurements, metadata, config)

  def handle_event(@validate_start, _measurements, metadata, config) do
    attributes = []

    OpentelemetryTelemetry.start_telemetry_span(
      config.tracer_id,
      :"graphql.validate",
      metadata,
      %{
        attributes: attributes
      }
    )
  end

  def handle_event(@validate_stop, _measurements, metadata, config) do
    OpentelemetryTelemetry.set_current_telemetry_span(config.tracer_id, metadata)

    case metadata.blueprint.execution.validation_errors do
      [] ->
        :ok

      errors ->
        message =
          errors
          |> Enum.uniq()
          |> Enum.map(&format_error/1)
          |> Jason.encode!(pretty: true)

        OpenTelemetry.Tracer.add_event(:"graphql.validation.error", message: message)
    end

    OpentelemetryTelemetry.end_telemetry_span(config.tracer_id, metadata)
  end

  defp format_error(%Absinthe.Phase.Error{locations: []} = error) do
    error_object = %{message: error.message}
    Map.merge(error.extra, error_object)
  end

  defp format_error(%Absinthe.Phase.Error{} = error) do
    error_object = %{
      message: error.message,
      locations: Enum.flat_map(error.locations, &format_location/1)
    }

    error_object =
      case error.path do
        [] -> error_object
        path -> Map.put(error_object, :path, path)
      end

    Map.merge(Map.new(error.extra), error_object)
  end

  defp format_location(%{line: line, column: col}) do
    [%{line: line || 0, column: col || 0}]
  end

  defp format_location(_), do: []
end
