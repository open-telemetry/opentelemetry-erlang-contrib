defmodule OpentelemetryGrpc.StatusCode do
  @moduledoc false

  # the semantic conventions name gRPC status codes the way the gRPC spec does
  # https://github.com/grpc/grpc/blob/master/doc/statuscodes.md
  @names %{
    0 => "OK",
    1 => "CANCELLED",
    2 => "UNKNOWN",
    3 => "INVALID_ARGUMENT",
    4 => "DEADLINE_EXCEEDED",
    5 => "NOT_FOUND",
    6 => "ALREADY_EXISTS",
    7 => "PERMISSION_DENIED",
    8 => "RESOURCE_EXHAUSTED",
    9 => "FAILED_PRECONDITION",
    10 => "ABORTED",
    11 => "OUT_OF_RANGE",
    12 => "UNIMPLEMENTED",
    13 => "INTERNAL",
    14 => "UNAVAILABLE",
    15 => "DATA_LOSS",
    16 => "UNAUTHENTICATED"
  }

  @spec name(non_neg_integer()) :: String.t()
  def name(code), do: Map.get(@names, code, "_OTHER")
end
