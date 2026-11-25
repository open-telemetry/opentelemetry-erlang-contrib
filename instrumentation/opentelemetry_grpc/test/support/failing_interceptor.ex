defmodule OpentelemetryGrpc.Test.FailingInterceptor do
  @moduledoc false

  # fails the call the way the given function does, before any request goes out
  @behaviour GRPC.Client.Interceptor

  @impl true
  def init(fail), do: fail

  @impl true
  def call(_stream, _request, _next, fail), do: fail.()
end
