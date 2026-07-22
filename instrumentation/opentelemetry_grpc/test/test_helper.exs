Application.ensure_all_started(:opentelemetry)
Application.ensure_all_started(:opentelemetry_api)

# grpc 0.x expects the host application to run the client connection
# supervisor; grpc 1.0 starts it in its own supervision tree
case DynamicSupervisor.start_link(strategy: :one_for_one, name: GRPC.Client.Supervisor) do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
end

ExUnit.start()
