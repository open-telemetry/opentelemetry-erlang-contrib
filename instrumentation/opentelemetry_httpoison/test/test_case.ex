defmodule OpentelemetryHTTPoison.Case do
  @moduledoc false

  use ExUnit.CaseTemplate

  setup do
    on_exit(fn ->
      Application.delete_env(:opentelemetry_httpoison, :infer_route)
      Application.delete_env(:opentelemetry_httpoison, :ot_attributes)
    end)
  end

  using do
    quote do
      defp set_env(key, value), do: Application.put_env(:opentelemetry_httpoison, key, value)
    end
  end
end
