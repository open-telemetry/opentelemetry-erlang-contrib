defmodule OpentelemetryNebulex.NebulexAttributes do
  @moduledoc false

  # Nebulex-specific OpenTelemetry Semantic Conventions attributes.
  #
  # Naming Conventions:
  # - Prefix with `nebulex.` to avoid conflicts with standard OTel attributes
  # - Use `snake_case` for attribute names
  # - Use dot notation for namespacing (e.g., `nebulex.cache`)

  @doc """
  The name of the Nebulex cache being accessed.
  """
  @spec nebulex_cache() :: :"nebulex.cache"
  def nebulex_cache, do: :"nebulex.cache"

  @doc """
  The keyslot module used for partitioned caches.
  """
  @spec nebulex_keyslot() :: :"nebulex.keyslot"
  def nebulex_keyslot, do: :"nebulex.keyslot"

  @doc """
  The cache model used by multilevel caches.
  """
  @spec nebulex_model() :: :"nebulex.model"
  def nebulex_model, do: :"nebulex.model"

  @doc """
  The result of a cache operation (`:hit` or `:miss`).
  """
  @spec nebulex_action() :: :"nebulex.action"
  def nebulex_action, do: :"nebulex.action"
end
