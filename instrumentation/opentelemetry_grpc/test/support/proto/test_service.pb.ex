defmodule Testserver.V1.HelloRequest do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field(:name, 1, type: :string)
end

defmodule Testserver.V1.HelloResponse do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field(:message, 1, type: :string)
end

defmodule Testserver.V1.NumberRequest do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field(:count, 1, type: :int32)
  field(:number, 2, type: :int32)
end

defmodule Testserver.V1.NumberResponse do
  @moduledoc false

  use Protobuf, protoc_gen_elixir_version: "0.15.0", syntax: :proto3

  field(:number, 1, type: :int32)
end

defmodule Testserver.V1.TestService.Service do
  @moduledoc false

  use GRPC.Service, name: "testserver.v1.TestService", protoc_gen_elixir_version: "0.15.0"

  rpc(:SayHello, Testserver.V1.HelloRequest, Testserver.V1.HelloResponse)

  rpc(:ListNumbers, Testserver.V1.NumberRequest, stream(Testserver.V1.NumberResponse))
end

defmodule Testserver.V1.TestService.Stub do
  @moduledoc false

  use GRPC.Stub, service: Testserver.V1.TestService.Service
end
