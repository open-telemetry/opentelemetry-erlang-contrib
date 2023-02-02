defmodule Telepoison.URI do
  @moduledoc """
  Exposes a function to normalise URIs in a format suitable for usage as Open Telemetry metadata.
  """

  alias HTTPoison.Request

  @default_route "/"

  @spec infer_route_from_request(Request.t()) :: binary()
  @doc """
  Infers the route of the provided `HTTPoison.Request`, returned in a format suitable for usage as Open Telemetry metadata.
  """
  def infer_route_from_request(%Request{url: url}) when is_binary(url), do: infer_route_from_url(url)

  def infer_route_from_request(%Request{}), do: @default_route

  @spec infer_route_from_url(binary()) :: binary()
  defp infer_route_from_url(url) when is_binary(url) do
    url
    |> URI.parse()
    |> Map.get(:path, "/")
    |> case do
      nil ->
        @default_route

      value ->
        case String.split(value, "/", parts: 2, trim: true) do
          [] ->
            @default_route

          [path] ->
            "/#{path}"

          [path, _] ->
            "/#{path}/:subpath"
        end
    end
  end
end
