opentelemetry_grpcbox
=====

This [grpcbox](https://github.com/tsloughter/grpcbox) interceptor uses the
[OpenTelemetry](http://opentelemetry.io/) API to create Spans for outgoing and
incoming requests and propagates them as part of the gRPC metadata.

### Server

Configure as an interceptor:

```erlang
#{service_protos => [route_guide_pb],
  unary_interceptor => {otel_grpcbox_interceptor, unary}}
```

Or as a middleware in the chain interceptor:

```erlang
#{service_protos => [route_guide_pb],
  unary_interceptor =>
    grpcbox_chain_interceptor:unary([..., 
                                     fun otel_grpcbox_interceptor:unary/4, 
                                     ...])}
```

### Client

Example in `sys.config` for setting up a [client
channel](https://grpc.io/docs/what-is-grpc/core-concepts/#channels):

``` erlang
{client, #{channels => [{default_channel, [{http, "localhost", 8080, []}], 
                        #{unary_interceptor => fun otel_grpcbox_interceptor:unary_client/7}}]}}
```
