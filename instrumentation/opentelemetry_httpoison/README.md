# OpentelemetryHTTPoison

[![Module Version](https://img.shields.io/hexpm/v/opentelemetry_httpoison.svg)](https://hex.pm/packages/opentelemetry_httpoison)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/opentelemetry_httpoison/)
[![Total Downloads](https://img.shields.io/hexpm/dt/opentelemetry_httpoison.svg)](https://hex.pm/packages/opentelemetry_httpoison)

OpentelemetryHTTPoison is a [opentelemetry-instrumented](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/glossary.md#instrumented-library) wrapper around HTTPoison.

## Usage

Replace usages of the `HTTPoison` module with `OpentelemetryHTTPoison` when calling one of the *derived* request functions provided by `HTTPoison` (`HTTPoison.get/3`, `HTTPoison.get!/3` etc.)

```elixir
# Before
HTTPoison.get!(url, headers, opts)

# After
OpentelemetryHTTPoison.get!(url, headers, opts)
```

## Configuration

OpentelemetryHTTPoison can be configured through `config :opentelemetry_httpoison`. The configurable options are:

* `:ot_attributes`: what default Open Telemetry metadata attributes will be sent per request

  If no value is provided, then no default Open Telemetry metadata attributes will sent per request by default

  If a `list` of two element `tuple`s (both elements of `String.t()`) is provided, then these will form the default Open Telemetry metadata attributes sent per request

  The first element of a provided `tuple` is the attribute name, e.g. `service.name`, whilst the second element is the attribute value, e.g. "shoppingcart"

* `:infer_route`: how the `http.route` Open Telemetry metadata will be set per request

  If no value is provided then the out of the box, conservative inference provided by `OpentelemetryHTTPoison.URI.infer_route_from_request/1` is used to determine the inference

  If a function with an arity of 1 (the argument given being the `t:HTTPoison.Request/0` `request`) is provided then that function is used to determine the inference

Both of these can be overridden per each call to OpentelemetryHTTPoison functions that wrap `OpentelemetryHTTPoison.request/1`, such as `OpentelemetryHTTPoison.get/3`, `OpentelemetryHTTPoison.get!/3`, `OpentelemetryHTTPoison.post/3` etc.

See here for [examples](#examples)

## Open Telemetry integration

Additionally, `OpentelemetryHTTPoison` provides some options that can be added to each derived function via
the `Keyword list` `opts` parameter (or the `t:HTTPoison.Request/0` `Keyword list` `options` parameter if calling `OpentelemetryHTTPoison.Request/1` directly). These are prefixed with `:ot_`.

* `:ot_span_name` - sets the span name.
* `:ot_attributes` - a list of `{name, value}` `tuple` attributes that will be added to the span.
* `:ot_resource_route` - sets the `http.route` attribute, depending on the value provided.

If the value is a string or an function with an arity of 1 (the `t:HTTPoison.Request/0` `request`) that is used to set the attribute

If `:infer` is provided, then the function discussed within the [Configuration](#configuration) section is used to set the attribute

If the atom `:ignore` is provided then the `http.route` attribute is ignored entirely

**It is highly recommended** to supply the `:ot_resource_route` explicitly as either a string or a function with an arity of 1 (the `t:HTTPoison.Request/0` `request`)

## Examples

In the below examples, `OpentelemetryHTTPoison.get!/3` is used for the sake of simplicity but other functions derived from `OpentelemetryHTTPoison.request/1` can be used

```elixir
config :opentelemetry_httpoison,
  ot_attributes: [{"service.name", "users"}]

OpentelemetryHTTPoison.get!(
  "https://www.example.com/user/list",
  [],
  ot_span_name: "list example users",
  ot_attributes: [{"example.language", "en"}],
  ot_resource_route: :infer
)
```

In the example above:

* OpentelemetryHTTPoison is configured with `{"service.name", "users"}` as the value for the `:ot_attributes` option
* `:infer` is passed as the value for the `:ot_resource_route` `Keyword list` option

Given the above, the `service.name` attribute will be set to "users" and the `http.route` attribute will be inferred as */user/:subpath*

```elixir
OpentelemetryHTTPoison.get!(
  "https://www.example.com/user/list",
  [],
  ot_span_name: "list example users",
  ot_attributes: [{"example.language", "en"}],
  ot_resource_route: :infer
)
```

In the example above:

* `:infer` is passed as the value for `:ot_resource_route` `Keyword list` option

Given the above, the `http.route` attribute will be inferred as */user/:subpath*

```elixir
config :opentelemetry_httpoison,
  infer_route: fn 
    %HTTPoison.Request{} = request -> URI.parse(request.url).path
  end

OpentelemetryHTTPoison.get!(
  "https://www.example.com/user/list",
  [],
  ot_resource_route: :infer
)
```

In the example above:

* OpentelemetryHTTPoison is configured with the `:infer_route` option set to a function which takes a `%HTTPoison.Request/0` argument, returning the path of the request URL
* `:infer` is passed as the value for `:ot_resource_route` `Keyword list` option

Given the above, the `http.route` attribute will be inferred as */user/list*

```elixir
OpentelemetryHTTPoison.get!(
  "https://www.example.com/user/list",
  [],
  ot_resource_route: "my secret path"
)
```

In the example above:

* `"my secret path"` is passed as the value for `:ot_resource_route` `Keyword list` option

Given the above, the `http.route` attribute will be set as *my secret path*

```elixir
OpentelemetryHTTPoison.get!(
  "https://www.example.com/user/list",
  [],
  ot_resource_route: :ignore
)
```

In the example above:

* `:ignore` is passed as the value for `:ot_resource_route` `Keyword list` option

Given the above, the `http.route` attribute will not be set to any value

## How it works

OpentelemetryHTTPoison, when executing an HTTP request to an external service, creates an OpenTelemetry span, injects
the [trace context propagation headers](https://www.w3.org/TR/trace-context/) in the request headers, and
ends the span once the response is received.
It automatically sets some of the [HTTP span attributes](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/semantic_conventions/http.md) like `http.status` etc,
based on the request and response data.

OpentelemetryHTTPoison by itself is not particularly useful: it becomes useful when used in conjunction with a "server-side"
opentelemetry-instrumented library, e.g. [opentelemetry_plug](https://github.com/opentelemetry-beam/opentelemetry_plug).
These do the opposite work: they take the trace context information from the request headers,
and they create a [SERVER](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/api.md#spankind) span which becomes the currently active span.

Using the two libraries together, it's possible to propagate trace information across several microservices and
through HTTP "jumps".

## Keep in mind

* The [Erlang opentelemetry SDK](https://github.com/open-telemetry/opentelemetry-erlang) stores
  the currently active span in a `pdict`, a per-process dict.
  If OpentelemetryHTTPoison is called from a different process than the one that initially handled the request and created
  the "server-side" span, OpentelemetryHTTPoison won't find a parent span and will create a new root client span,
  losing the trace context.
  In this case, your only option to correctly propagate the trace context is to manually pass around the parent
  span, and pass it to OpentelemetryHTTPoison when doing the HTTP client request.

* If the request fails due to nxdomain, the `process_response_status_code` hook is not called and therefore
  the span is not ended.

## What's missing

* Set [SpanKind](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/trace/api.md#spankind) to client
* Support for explicit parent span
* Support for fixed span attributes
* Lots of other stuff...
