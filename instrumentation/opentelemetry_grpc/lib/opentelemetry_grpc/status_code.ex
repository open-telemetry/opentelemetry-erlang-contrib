defmodule OpentelemetryGrpc.StatusCode do
  @moduledoc false

  alias OpenTelemetry.SemConv.Incubating.RPCAttributes

  @names Map.new(RPCAttributes.rpc_grpc_status_code_values(), fn {name, code} ->
           {code, name |> Atom.to_string() |> String.upcase()}
         end)

  @spec name(non_neg_integer()) :: String.t()
  def name(code), do: Map.get(@names, code, "_OTHER")
end
