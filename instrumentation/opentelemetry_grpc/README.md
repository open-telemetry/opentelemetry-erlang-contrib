# OpentelemetryGrpc

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_grpc)](https://hex.pm/packages/opentelemetry_grpc)

OpenTelemetry instrumentation for gRPC clients and servers in Elixir, enabling distributed tracing, error tracking, and context propagation across services.

This library provides comprehensive tracing for gRPC applications, supporting both client and server instrumentation with proper context propagation according to OpenTelemetry semantic conventions.

## Features

- **Client Instrumentation**: Automatic tracing of outgoing gRPC requests
- **Server Instrumentation**: Automatic tracing of incoming gRPC requests  
- **Context Propagation**: Proper trace context propagation between services
- **Error Handling**: Comprehensive error tracking and status reporting using gRPC status codes
- **Semantic Conventions**: Follows OpenTelemetry semantic conventions for RPC

## Installation

Add `opentelemetry_grpc` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:opentelemetry_grpc, "~> 1.0"}
  ]
end
```

## Usage

### Basic Setup

For most applications, you can set up both client and server instrumentation:

```elixir
# In your application startup (e.g., Application.start/2)
OpentelemetryGrpc.setup()
```

### Client Instrumentation

To instrument only gRPC client calls:

```elixir
OpentelemetryGrpc.Client.setup()
```

For context propagation in client requests, add the interceptor to your gRPC channel. The OpenTelemetry interceptor should typically be the first in the list to ensure proper context propagation:

```elixir
{:ok, channel} = GRPC.Stub.connect("localhost:50051",
  interceptors: [OpentelemetryGrpc.ContextPropagatorInterceptor])

# Use the channel for your gRPC calls
MyService.Stub.my_method(channel, request)
```

### Server Instrumentation

To instrument only gRPC server requests:

```elixir
OpentelemetryGrpc.Server.setup()
```

By default, server instrumentation includes context propagation from incoming request headers. To control span relationships:

```elixir
# Create child spans (default) - spans should be part of the same trace
OpentelemetryGrpc.Server.setup(span_relationship: :child)

# Create span links instead of parent-child relationships - use for fire-and-forget or detached async processing
OpentelemetryGrpc.Server.setup(span_relationship: :link)

# Disable context propagation entirely - useful for isolated traces
OpentelemetryGrpc.Server.setup(span_relationship: :none)
```

### Advanced Configuration

You can pass server-specific options through the main setup function:

```elixir
OpentelemetryGrpc.setup(server: [span_relationship: :link])
```

## Context Propagation

The library supports OpenTelemetry context propagation:

- **Client**: Use `OpentelemetryGrpc.ContextPropagatorInterceptor` to inject trace context into outgoing requests
- **Server**: Context is automatically extracted from incoming request headers (when enabled)
