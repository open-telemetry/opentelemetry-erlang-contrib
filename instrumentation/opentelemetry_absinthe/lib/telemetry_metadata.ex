defmodule OpentelemetryAbsinthe.TelemetryMetadata do
  @moduledoc """
    A helper module to allow integrators to add custom data to their context
    which will then be added to the [:opentelemetry_absinthe, :graphql, :handled]
    event
  """
  @key __MODULE__

  @type absinthe_context :: map()
  @type telemetry_metadata :: %{
          optional(atom()) => any()
        }

  @spec update_context(absinthe_context(), telemetry_metadata()) :: absinthe_context()
  def update_context(%{} = context, %{} = metadata),
    do: Map.update(context, @key, metadata, &Map.merge(&1, metadata))

  @spec from_context(absinthe_context()) :: telemetry_metadata()
  def from_context(%{} = context), do: Map.get(context, @key, %{})
end
