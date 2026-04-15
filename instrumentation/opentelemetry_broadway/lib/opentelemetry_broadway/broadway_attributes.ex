defmodule OpentelemetryBroadway.BroadwayAttributes do
  @moduledoc """
  OpenTelemetry span attributes specific to the Broadway instrumentation.

  These attributes are not part of the OpenTelemetry Semantic Conventions.
  """

  @doc """
  The number of messages in the batch that completed successfully.

  ### Value type

  Value must be of type `non_neg_integer()`.
  """
  @spec messaging_broadway_batch_successful_count() ::
          :"messaging.broadway.batch.successful_count"
  def messaging_broadway_batch_successful_count do
    :"messaging.broadway.batch.successful_count"
  end

  @doc """
  The number of messages in the batch that failed.

  ### Value type

  Value must be of type `non_neg_integer()`.
  """
  @spec messaging_broadway_batch_failed_count() :: :"messaging.broadway.batch.failed_count"
  def messaging_broadway_batch_failed_count do
    :"messaging.broadway.batch.failed_count"
  end
end
