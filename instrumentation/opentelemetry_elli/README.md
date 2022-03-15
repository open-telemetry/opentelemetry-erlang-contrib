# opentelemetry_elli

![Common Test](https://github.com/opentelemetry-beam/opentelemetry_elli/workflows/Common%20Test/badge.svg) [![Gitter](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Elli middleware for OpenTelemetry instrumentation.

## Setup and Configuration

``` erlang
{deps, [opentelemetry_elli]}.
```

Using the elli_middleware callback place oc_elli_middelware as the first module to be called in the list of handlers:

``` erlang
[{callback, elli_middleware},
 {callback_args, [{mods, [{otel_elli_middleware, []},
                          {<YOUR HANDLER>, []}]}]}]
```



OpenTelemetry's [HTTP Semantic Conventions](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md#http-server) for a server details the attributes added to a Span automatically by using this middleware. One such attribute must be set in your `sys.config` under the `opentelemetry_elli` application like:

``` erlang
{opentelemetry_elli, [{server_name, <<"my-http-server">>}]}.
```

It is strongly recommended to set this environment variable so the attribute can be included:

> http.server_name has shown great value in practice, as bogus HTTP Host headers occur often in the wild. It is strongly recommended to set http.server_name to allow associating requests with some logical server entity.

## Use

### Including the Middleware and setting Span names

The middleware takes care of extracting the parent Span from the requests
headers, both the [W3C](https://w3c.github.io/trace-context/) and [B3](https://github.com/openzipkin/b3-propagation) formats are supported.

Because Elli has no router there is not a way to get a very descriptive Span
name automatically. See the OpenTelemetry docs [Semantic conventions for HTTP spans](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md#name) for
why you don't want to set the Span name to the raw path of the request. Thus,
the Span has the name `"HTTP {METHOD_NAME}"`.

The macro `update_name` from `opentelemetry_api/include/otel_tracer.hrl` allows you
to update the name of the Span after it has started:

``` erlang
handle(Req, Args) ->
    handle(Req#req.path, Req, Args).

handle([<<"hello">>, Who], Req, _Args) ->
    ?update_name(<<"/hello/{who}">>),
    {ok, [], <<"Hello ", Who/binary>>}.
```

Attributes set by the middleware can be found in the OpenTelemetry docs [Semantic
conventions for HTTP spans](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/data-http.md).

Note that it isn't required to update the Span name, that is just a suggestion
for when it makes sense to do so. And another option is to create a child Span
with this better name and its parent will be the Span named `"HTTP
{METHOD_NAME}"`.

### Excluding Paths

Not all paths are created equal. It is likely you don't want to Trace a health
check endpoint or `/metrics` if using Prometheus to scrape metrics. So this
library offers two ways to exclude requests from potentially creating Spans by
filtering on the raw path in the URL.

A Application environment variable is read in on Elli start, so the follow can
be added to `sys.config` to exclude the url `/exclude/me`:

``` erlang

{opentelemetry_elli, [{excluded_urls, ["/health", "/metrics"]}]}
```

An OS environment variable, `OTEL_ELLI_EXCLUDED_URLS`, is also read and is
a comma separated list of paths.

The lists from both are merged.

## Testing

A Common Test suite which starts an Elli server and tests the exported Spans created by a test handle is found under `test/`. Run with:

``` erlang
$ rebar3 ct
```
