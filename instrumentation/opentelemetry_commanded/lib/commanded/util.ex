defmodule OpentelemetryCommanded.Util do
  @moduledoc false

  def safe_context_propagation(trace_ctx) when is_nil(trace_ctx) do
    nil
  end

  def safe_context_propagation(trace_ctx) do
    trace_ctx
    |> decode_headers()
    |> :otel_propagator_text_map.extract()
  end

  def encode_headers(headers), do: Enum.map(headers, &Tuple.to_list/1)

  def decode_headers(headers), do: Enum.map(headers, &List.to_tuple/1)

  def encode_ctx(:undefined), do: :undefined
  def encode_ctx(ctx), do: Tuple.to_list(ctx)

  def decode_ctx("undefined"), do: :undefined
  def decode_ctx(:undefined), do: :undefined

  def decode_ctx(ctx) do
    Enum.map(ctx, fn
      el when is_binary(el) -> String.to_existing_atom(el)
      el -> el
    end)
    |> List.to_tuple()
  end

  def struct_name(%name{}) do
    name
  end
end
