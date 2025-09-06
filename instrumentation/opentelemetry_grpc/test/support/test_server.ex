defmodule OpentelemetryGrpc.Test.TestServer do
  use GRPC.Server, service: Testserver.V1.TestService.Service

  alias Testserver.V1.{
    HelloResponse,
    NumberResponse
  }

  def say_hello(request, _stream) do
    case request.name do
      "TriggerNotFound" ->
        raise GRPC.RPCError, status: :not_found, message: "User not found"

      "TriggerInvalidArgument" ->
        raise GRPC.RPCError, status: :invalid_argument, message: "Invalid name format"

      "TriggerException" ->
        raise RuntimeError, "Server crashed"

      _ ->
        %HelloResponse{message: "Hello, #{request.name}!"}
    end
  end

  def list_numbers(request, stream) do
    1..request.count
    |> Enum.each(fn i ->
      response = %NumberResponse{number: i * request.number}
      GRPC.Server.send_reply(stream, response)
    end)
  end
end
