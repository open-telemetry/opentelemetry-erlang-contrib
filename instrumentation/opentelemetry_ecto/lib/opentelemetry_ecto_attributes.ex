defmodule OpentelemetryEcto.EctoAttributes do
  @moduledoc """
  OpenTelemetry Semantic Conventions for Ecto attributes.
  """

  @doc """
  The time spent decoding the data received from the database in seconds.

  ### Value type

  Value must be of type `float()`.
  """
  @spec ecto_decode_time_duration :: :"ecto.decode_time.duration"
  def ecto_decode_time_duration do
    :"ecto.decode_time.duration"
  end

  @doc """
  The time the connection spent waiting before being checked out for the query in seconds.

  ### Value type

  Value must be of type `float()`.
  """
  @spec ecto_idle_time_duration :: :"ecto.idle_time.duration"
  def ecto_idle_time_duration do
    :"ecto.idle_time.duration"
  end

  @doc """
  The time spent waiting to check out a database connection in seconds.

  ### Value type

  Value must be of type `float()`.
  """
  @spec ecto_queue_time_duration :: :"ecto.queue_time.duration"
  def ecto_queue_time_duration do
    :"ecto.queue_time.duration"
  end

  @doc """
  The time spent executing the query in seconds.

  ### Value type

  Value must be of type `float()`.
  """
  @spec ecto_query_time_duration :: :"ecto.query_time.duration"
  def ecto_query_time_duration do
    :"ecto.query_time.duration"
  end

  @doc """
  The sum of (queue_time, query_time, and decode_time)️ in seconds.

  ### Value type

  Value must be of type `float()`.
  """
  @spec ecto_total_time_duration :: :"ecto.total_time.duration"
  def ecto_total_time_duration do
    :"ecto.total_time.duration"
  end
end
