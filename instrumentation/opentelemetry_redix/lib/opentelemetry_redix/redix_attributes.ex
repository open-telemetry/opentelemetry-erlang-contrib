defmodule OpentelemetryRedix.RedixAttributes do
  @moduledoc """
  OpenTelemetry span attributes specific to the Redix instrumentation.

  These attributes are not part of the OpenTelemetry Semantic Conventions.
  """

  @doc """
  The Redis simple error prefix returned when an operation fails (e.g. `ERR`, `WRONGTYPE`).
  """
  @spec redix_response_status_code() :: :"redix.response.status_code"
  def redix_response_status_code, do: :"redix.response.status_code"

  @doc """
  The name of the Redix connection.
  """
  @spec redix_connection_name() :: :"redix.connection.name"
  def redix_connection_name, do: :"redix.connection.name"
end
