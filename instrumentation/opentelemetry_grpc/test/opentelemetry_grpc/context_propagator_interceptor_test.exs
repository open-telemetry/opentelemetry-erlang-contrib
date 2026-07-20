defmodule OpentelemetryGrpc.ContextPropagatorInterceptorTest do
  use ExUnit.Case, async: false
  doctest OpentelemetryGrpc.ContextPropagatorInterceptor

  require OpenTelemetry.Tracer
  require OpenTelemetry.Span

  alias OpentelemetryGrpc.ContextPropagatorInterceptor
  alias Testserver.V1.HelloRequest

  setup do
    :application.stop(:opentelemetry)
    :application.set_env(:opentelemetry, :tracer, :otel_tracer_default)

    :application.set_env(:opentelemetry, :processors, [
      {:otel_batch_processor, %{scheduled_delay_ms: 1}}
    ])

    :application.start(:opentelemetry)

    :ok
  end

  describe "init/1" do
    test "returns options unchanged" do
      opts = [some: "option", other: 123]
      assert ContextPropagatorInterceptor.init(opts) == opts
    end

    test "returns empty list for no options" do
      assert ContextPropagatorInterceptor.init([]) == []
    end
  end

  describe "call/4" do
    test "calls next function with stream" do
      stream = %GRPC.Client.Stream{}
      request = %HelloRequest{name: "test"}

      result = ContextPropagatorInterceptor.call(stream, request, next_ok(), [])

      assert {:ok, %GRPC.Client.Stream{}} = result
    end

    test "preserves existing headers" do
      existing_headers = %{"existing" => "header", "another" => "value"}
      stream = %GRPC.Client.Stream{headers: existing_headers}
      request = %HelloRequest{name: "test"}

      result = ContextPropagatorInterceptor.call(stream, request, next_ok(), [])

      assert {:ok, modified_stream} = result
      assert modified_stream.headers == existing_headers
    end
  end

  defp next_ok, do: fn stream_arg, _request -> {:ok, stream_arg} end
end
