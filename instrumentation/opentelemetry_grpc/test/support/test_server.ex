defmodule OpentelemetryGrpc.Test.TestServer do
  use GRPC.Server, service: Testserver.V1.TestService.Service

  alias Testserver.V1.{
    HelloResponse,
    NumberResponse
  }

  def say_hello(%{name: "TriggerOK"}, _stream) do
    %HelloResponse{message: "OK"}
  end

  def say_hello(%{name: "TriggerCancelled"}, _stream) do
    raise GRPC.RPCError, status: :cancelled, message: "Client cancelled the request"
  end

  def say_hello(%{name: "TriggerUnknown"}, _stream) do
    raise GRPC.RPCError, status: :unknown, message: "Unknown error"
  end

  def say_hello(%{name: "TriggerInvalidArgument"}, _stream) do
    raise GRPC.RPCError, status: :invalid_argument, message: "Invalid argument"
  end

  def say_hello(%{name: "TriggerDeadlineExceeded"}, _stream) do
    raise GRPC.RPCError, status: :deadline_exceeded, message: "Deadline exceeded"
  end

  def say_hello(%{name: "TriggerNotFound"}, _stream) do
    raise GRPC.RPCError, status: :not_found, message: "Not found"
  end

  def say_hello(%{name: "TriggerAlreadyExists"}, _stream) do
    raise GRPC.RPCError, status: :already_exists, message: "Already exists"
  end

  def say_hello(%{name: "TriggerPermissionDenied"}, _stream) do
    raise GRPC.RPCError, status: :permission_denied, message: "Permission denied"
  end

  def say_hello(%{name: "TriggerResourceExhausted"}, _stream) do
    raise GRPC.RPCError, status: :resource_exhausted, message: "Resource exhausted"
  end

  def say_hello(%{name: "TriggerFailedPrecondition"}, _stream) do
    raise GRPC.RPCError, status: :failed_precondition, message: "Failed precondition"
  end

  def say_hello(%{name: "TriggerAborted"}, _stream) do
    raise GRPC.RPCError, status: :aborted, message: "Aborted"
  end

  def say_hello(%{name: "TriggerOutOfRange"}, _stream) do
    raise GRPC.RPCError, status: :out_of_range, message: "Out of range"
  end

  def say_hello(%{name: "TriggerUnimplemented"}, _stream) do
    raise GRPC.RPCError, status: :unimplemented, message: "Unimplemented"
  end

  def say_hello(%{name: "TriggerInternal"}, _stream) do
    raise GRPC.RPCError, status: :internal, message: "Internal error"
  end

  def say_hello(%{name: "TriggerUnavailable"}, _stream) do
    raise GRPC.RPCError, status: :unavailable, message: "Service unavailable"
  end

  def say_hello(%{name: "TriggerDataLoss"}, _stream) do
    raise GRPC.RPCError, status: :data_loss, message: "Data loss"
  end

  def say_hello(%{name: "TriggerUnauthenticated"}, _stream) do
    raise GRPC.RPCError, status: :unauthenticated, message: "Unauthenticated"
  end

  def say_hello(%{name: "TriggerException"}, _stream) do
    raise RuntimeError, "Server crashed"
  end

  def say_hello(%{name: name}, _stream) do
    %HelloResponse{message: "Hello, #{name}!"}
  end

  def list_numbers(request, stream) do
    1..request.count
    |> Enum.each(fn i ->
      response = %NumberResponse{number: i * request.number}
      GRPC.Server.send_reply(stream, response)
    end)
  end
end
