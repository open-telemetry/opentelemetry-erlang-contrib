defmodule OpentelemetryGrpc.ContextPropagatorInterceptor do
  @moduledoc """
  gRPC client interceptor for OpenTelemetry context propagation.

  This interceptor injects the current OpenTelemetry trace context into outgoing
  gRPC client requests by adding distributed tracing headers to the gRPC metadata.
  This enables trace continuity across service boundaries in distributed systems.

  ## Usage

  Add this interceptor to your gRPC client channel:

      {:ok, channel} = GRPC.Stub.connect("localhost:50051",
        interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor])

  ## Interceptor Ordering

  This interceptor can be placed anywhere in the interceptor pipeline. In practice,
  the position shouldn't matter as long as other interceptors don't drop or modify
  the injected tracing headers. When in doubt, place it at the end of the pipeline
  to ensure the headers are added after any other metadata modifications.

  ### Example with Multiple Interceptors

      {:ok, channel} = GRPC.Stub.connect("localhost:50051",
        interceptors: [
          MyCustomInterceptor,
          OpentelemetryGrpc.ContextPropagatorInterceptor  # Safe at the end
        ])

  """

  alias GRPC.Client.Stream

  @behaviour GRPC.Client.Interceptor

  @impl GRPC.Client.Interceptor
  def init(opts), do: opts

  @impl GRPC.Client.Interceptor
  def call(stream, req, next, _opts) do
    metadata =
      :opentelemetry.get_text_map_injector()
      |> :otel_propagator_text_map.inject(%{}, &Map.put(&3, &1, &2))

    stream
    |> Stream.put_headers(metadata)
    |> next.(req)
  end
end
