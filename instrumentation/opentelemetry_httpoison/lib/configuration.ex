defmodule OpentelemetryHTTPoison.Configuration do
  @moduledoc false

  require Logger

  @spec setup(any) :: nil
  def setup(opts \\ []) do
    errors = []

    opts_infer_route = Keyword.get(opts, :infer_route)

    if opts_infer_route != nil do
      set(:infer_route, opts_infer_route)
    end

    route_inference_fn = get(:infer_route)

    errors =
      add_if(
        errors,
        not is_function(route_inference_fn, 1),
        [
          "\nThe configured :infer_route keyword option value must be a function with an arity of 1"
        ]
      )

    opts_ot_attributes = Keyword.get(opts, :ot_attributes)

    if opts_ot_attributes != nil do
      set(:ot_attributes, opts_ot_attributes)
    end

    ot_attributes = get(:ot_attributes)

    errors =
      add_if(
        errors,
        not is_list(ot_attributes),
        ["\nThe configured :ot_attributes option must be a [{key, value}] list"]
      )

    case errors do
      [] -> nil
      _ -> raise ArgumentError, Enum.join(errors)
    end
  end

  @doc """
  Get a configured option
  """
  @spec get(:infer_route | :ot_attributes) :: any()
  def get(key)

  def get(:infer_route),
    do:
      Application.get_env(
        :opentelemetry_httpoison,
        :infer_route,
        &OpentelemetryHTTPoison.URI.infer_route_from_request/1
      )

  def get(:ot_attributes), do: Application.get_env(:opentelemetry_httpoison, :ot_attributes, [])

  defp set(key, value), do: Application.put_env(:opentelemetry_httpoison, key, value)

  defp add_if(list, condition, value)
  defp add_if(list, true, value), do: [value | list]
  defp add_if(list, false, _), do: list
end
