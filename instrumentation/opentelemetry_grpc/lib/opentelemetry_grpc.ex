defmodule OpentelemetryGrpc do
  alias OpentelemetryGrpc.Client
  alias OpentelemetryGrpc.Server

  @options_schema NimbleOptions.new!(
                    server: [
                      type:
                        {:or,
                         [
                           {:keyword_list, Server.options_schema()},
                           {:in, [:disabled]}
                         ]},
                      default: [],
                      doc: """
                      Server instrumentation options. Set to `:disabled` to skip server setup. \n\n #{NimbleOptions.docs(Server.options_schema(), nest_level: 1)}
                      """
                    ],
                    client: [
                      type: {:in, [[], :disabled]},
                      default: [],
                      doc: """
                      Client instrumentation. Set to `:disabled` to skip client setup.
                      """
                    ]
                  )

  @moduledoc """
  OpenTelemetry instrumentation for gRPC clients and servers.

  This library provides OpenTelemetry tracing for gRPC applications, supporting
  both client and server instrumentation with proper context propagation.

  ## Client Instrumentation

  To instrument gRPC client calls, set up the client telemetry handler:

      OpentelemetryGrpc.Client.setup()

  For context propagation in client requests, add the interceptor to your gRPC channel:

      {:ok, channel} = GRPC.Stub.connect("localhost:50051",
        interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor]
      )

  ## Server Instrumentation

  To instrument gRPC server requests, set up the server telemetry handler:

      OpentelemetryGrpc.Server.setup()

  By default, server instrumentation includes context propagation. To control span relationships:

      OpentelemetryGrpc.Server.setup(span_relationship: :link)

  ## Complete Setup

  To set up both client and server instrumentation:

      OpentelemetryGrpc.setup()

  This is equivalent to calling both `Client.setup()` and `Server.setup()`.

  """

  @doc """
  Set up both client and server gRPC instrumentation.

  ## Options

  #{NimbleOptions.docs(@options_schema)}

  ## Examples

      # Basic setup
      OpentelemetryGrpc.setup()

      # With server options
      OpentelemetryGrpc.setup(server: [span_relationship: :link])

      # Disable client instrumentation
      OpentelemetryGrpc.setup(client: :disabled)

  """
  @spec setup(unquote(NimbleOptions.option_typespec(@options_schema))) :: :ok
  def setup(opts \\ []) do
    config =
      opts
      |> NimbleOptions.validate!(@options_schema)
      |> Enum.into(%{})

    if config.server != :disabled do
      Server.setup(config.server)
    end

    if config.client != :disabled do
      Client.setup(config.client)
    end

    :ok
  end
end
