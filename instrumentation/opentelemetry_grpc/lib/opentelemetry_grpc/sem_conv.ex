defmodule OpentelemetryGrpc.SemConv do
  @moduledoc false

  # the RPC conventions went stable after v1.37.0, and existing instrumentations
  # keep emitting the experimental ones unless OTEL_SEMCONV_STABILITY_OPT_IN asks
  # https://github.com/open-telemetry/semantic-conventions/blob/main/docs/non-normative/rpc-migration.md

  alias OpenTelemetry.SemConv.Incubating.RPCAttributes
  alias OpenTelemetry.SemConv.NetworkAttributes
  alias OpentelemetryGrpc.StatusCode

  @type stability :: :experimental | :stable | :dup

  @spec stability() :: stability()
  def stability do
    opt_ins =
      "OTEL_SEMCONV_STABILITY_OPT_IN"
      |> System.get_env("")
      |> String.split(",", trim: true)
      |> Enum.map(&String.trim/1)

    to_stability("rpc/dup" in opt_ins, "rpc" in opt_ins)
  end

  # rpc/dup takes precedence over rpc
  defp to_stability(true, _rpc), do: :dup
  defp to_stability(false, true), do: :stable
  defp to_stability(false, false), do: :experimental

  @spec rpc_attributes(stability(), String.t(), String.t()) :: map()
  def rpc_attributes(:experimental, service, method) do
    %{
      RPCAttributes.rpc_system() => RPCAttributes.rpc_system_values().grpc,
      RPCAttributes.rpc_service() => service,
      RPCAttributes.rpc_method() => method,
      NetworkAttributes.network_transport() => "tcp"
    }
  end

  # opentelemetry_semantic_conventions does not ship the stable RPC keys yet
  def rpc_attributes(:stable, service, method) do
    %{
      :"rpc.system.name" => "grpc",
      RPCAttributes.rpc_method() => "#{service}/#{method}"
    }
  end

  # both conventions name the method rpc.method, so the stable value wins
  def rpc_attributes(:dup, service, method) do
    Map.merge(
      rpc_attributes(:experimental, service, method),
      rpc_attributes(:stable, service, method)
    )
  end

  @spec status_code_attributes(stability(), non_neg_integer()) :: map()
  def status_code_attributes(:experimental, status_code) do
    %{RPCAttributes.rpc_grpc_status_code() => status_code}
  end

  def status_code_attributes(:stable, status_code) do
    %{:"rpc.status_code" => StatusCode.name(status_code)}
  end

  def status_code_attributes(:dup, status_code) do
    Map.merge(
      status_code_attributes(:experimental, status_code),
      status_code_attributes(:stable, status_code)
    )
  end
end
