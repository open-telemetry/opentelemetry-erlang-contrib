defmodule OpentelemetryGrpc.TestSupport do
  @default_port 42069

  def start_server(port \\ @default_port) do
    {:ok, _pid, actual_port} =
      GRPC.Server.start(
        OpentelemetryGrpc.Test.TestServer,
        port,
        adapter: GRPC.Server.Adapters.Cowboy
      )

    actual_port
  end

  def stop_server(_port) do
    GRPC.Server.stop(OpentelemetryGrpc.Test.TestServer, adapter: GRPC.Server.Adapters.Cowboy)
  end

  def connect_client(port \\ @default_port) do
    GRPC.Stub.connect("127.0.0.1:#{port}",
      interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
    )
  end

  def connect_client(port, opts) when is_integer(port) do
    interceptors =
      Keyword.get(opts, :interceptors, [OpentelemetryGrpc.ContextPropagatorInterceptor])

    GRPC.Stub.connect("127.0.0.1:#{port}", interceptors: interceptors)
  end

  def disconnect_client(channel) do
    GRPC.Stub.disconnect(channel)
  end
end
