# Hooks and Callback Options

Rules for any configuration option whose value is user-supplied code. These
apply to every instrumentation package in this repository, not only HTTP ones.

For the reasoning behind each rule, see
[Understanding Hooks and Callback Options](../explanation/hooks.md).

## Callbacks Must Be Setup-Time Options

User code that needs to participate in span creation MUST be passed as an
option to the package's `setup/1`. It MUST NOT be expected to work as a second
`:telemetry` handler attached to the same event.

See [Why a second handler cannot work](../explanation/hooks.md#why-a-second-handler-cannot-work).

## Callbacks Receive One Map

Every callback takes exactly one argument: a context map.

```erlang
hook(#{phase := start, meta := #{req := Req}}) ->
    ?set_attributes(#{'com.acme.tenant' => tenant(Req)});
hook(_Context) ->
    ok.
```

```elixir
def hook(%{phase: :start, meta: %{req: req}}) do
  OpenTelemetry.Tracer.set_attributes(%{"com.acme.tenant": tenant(req)})
end

def hook(_context), do: :ok
```

Packages MUST NOT define callbacks with positional parameters, and MUST NOT add
a trailing user-arguments slot. Values that are present on every invocation,
such as `phase` and `event`, stay inside the map and are not promoted to
positional parameters.

Package documentation MUST lead with `phase` in its hook examples.

See [Why one map](../explanation/hooks.md#why-one-map) and
[Why nothing is promoted out of the map](../explanation/hooks.md#why-nothing-is-promoted-out-of-the-map).

### Context Map Keys

| Key | Value |
| --- | ----- |
| `event` | the Telemetry event name that triggered the callback |
| `phase` | the normalized span lifecycle moment, `start` or `stop` |
| `meta` | the Telemetry event metadata |
| `measurements` | the Telemetry measurements, where the event has them |
| `config` | the resolved instrumentation configuration |
| `span_ctx` | the span this callback is operating on, where one exists |

Packages populate the keys that apply to the callback in question and MAY add
package-specific keys.

### Stability

These guarantees are binding.

The context map is additive-only. Packages MAY add keys in a minor release.
Packages MUST NOT remove or rename a key outside a major release.

Callbacks SHOULD match only the keys they use. Callers MUST NOT depend on the
map's exact key set, for example by enumerating keys or checking its size.

Packages MAY add `event` and `phase` values in a minor release, so callbacks
MUST NOT assume the sets are closed.

### Comparators

Callbacks that compare two values of the same kind, such as the header sort
functions consumed by `otel_http`, take those two values positionally. The
context map does not apply to them.

## Callback Shape

Callbacks MUST accept an external function capture:

```erlang
setup(#{hook => fun my_mod:hook/1})
```

```elixir
setup(hook: &MyMod.hook/1)
```

Packages SHOULD NOT accept `{M, F, A}` tuples for new options, and SHOULD NOT
require literal anonymous functions or local captures such as `fun on_start/1`.
User state belongs in the callback's own module or in `config`.

See [Why external captures](../explanation/hooks.md#why-external-captures).

## Metadata Shape Varies By Event

Documentation for each hook MUST state the shape of `meta` per event.

In `opentelemetry_cowboy`, `[cowboy, request, start]` metadata carries `req`,
while `[cowboy, request, early_error]` metadata carries `partial_req` and no
`req` key.

## Resolution Time

Callbacks and all other options MUST be resolved once during `setup/1` and
stored in the handler configuration passed to `telemetry:attach_many/4`.

Packages MUST NOT read `application:get_env`, `Application.get_env`, or
`System.get_env` per request or per event, and MUST NOT re-run option
validation per request.

## Error Containment

Every invocation of user-supplied code MUST be wrapped so that an exception
cannot escape into the Telemetry handler.

```erlang
run_hook(undefined, _Context) ->
    ok;
run_hook(Hook, Context) ->
    try
        Hook(Context)
    catch
        Kind:Reason:Stacktrace ->
            ?LOG_WARNING("`hook` raised ~tp:~tp. Ignoring.~n~tp",
                         [Kind, Reason, Stacktrace]),
            ok
    end.
```

A callback that returns an unusable value MUST be handled the same way: log once
and fall back to the default. Packages MUST NOT raise on a bad return value.

See [Why error containment is mandatory](../explanation/hooks.md#why-error-containment-is-mandatory).

## The Hook Option

Packages expose exactly one hook option, named `hook`. It is invoked at every
span lifecycle point the package instruments, runs with the span active, and its
return value is ignored. Inside it, users may call `set_attributes`, `add_event`,
`update_name`, `set_status`, or any other span operation.

```erlang
setup(#{hook => fun my_mod:hook/1}).
```

```erlang
hook(#{phase := start, meta := #{req := Req}}) ->
    ?set_attributes(#{'com.acme.tenant' => tenant(Req)});
hook(#{phase := stop}) ->
    ?add_event('com.acme.completed', #{});
hook(_Context) ->
    ok.
```

Packages MUST NOT add per-phase hook options such as `span_start_hook`,
`request_hook`, or `response_hook`.

See [Why a single hook option](../explanation/hooks.md#why-a-single-hook-option).

### Phase And Event

`event` is the package's own Telemetry event name. `phase` is the normalized
span lifecycle moment and is only ever `start` or `stop`. Packages MUST map
every instrumented event onto a phase. Exceptions map to `stop`; there is no
`exception` phase.

`opentelemetry_cowboy`:

| Event | Phase | Notes |
| ----- | ----- | ----- |
| `[cowboy, request, start]` | `start` | |
| `[cowboy, request, stop]` | `stop` | |
| `[cowboy, request, exception]` | `stop` | `meta` carries `kind`, `reason`, and `stacktrace` |
| `[cowboy, request, early_error]` | `start`, then `stop` | the span is created and ended inside one handler call, so the hook is invoked twice |

See [Why exceptions are a stop phase](../explanation/hooks.md#why-exceptions-are-a-stop-phase).

### A Catch-All Clause Is Required

User hooks MUST end with a clause that matches any context:

```erlang
hook(_Context) -> ok.
```

Package documentation MUST show the catch-all clause in its hook examples.

See [Why a catch-all clause](../explanation/hooks.md#why-a-catch-all-clause).

### Hooks Are Not Configuration Callbacks

A hook produces side effects and its return value is ignored. A configuration
callback returns a value the instrumentation acts on, such as
`public_endpoint_fn` returning a boolean or `operation_parser` returning a
parsed tuple.

Configuration callbacks keep their own descriptive option names and are not
folded into `hook`. Both follow the context map and shape rules.

### Hooks Cannot Influence Sampling

A hook runs after the span exists, therefore after the head sampling decision.
Samplers receive only the initial attribute set passed at span creation, so
attributes set from a hook are invisible to them.

Where the HTTP semantic conventions list attributes as sampling-relevant,
instrumentation provides them at creation time.

See [Sampling and hooks](../explanation/hooks.md#sampling-and-hooks).

### Hooks Can Overwrite Instrumented Attributes

A hook runs after the instrumentation has set its own attributes, so a
`set_attributes` call naming the same key wins. This is the opposite of the
precedence rule for `extra_attrs`, where instrumented attributes take
precedence.

Hook documentation MUST warn against writing keys the instrumentation already
owns, and MUST point users at their own attribute namespace. The `otel.*`
namespace is reserved and existing semantic convention namespaces are not
reused. Application attributes belong under a reverse domain name or an
application prefix, for example `com.acme.tenant`.

### Hooks Decide Span Status

A status set from a hook wins over the status the instrumentation would set.

```erlang
hook(#{phase := stop, meta := #{resp_status := 402}, span_ctx := SpanCtx}) ->
    otel_span:set_status(SpanCtx, opentelemetry:status(?OTEL_STATUS_ERROR, <<"payment required">>));
hook(_Context) ->
    ok.
```

On every path that ends a span, packages MUST run the `stop` hook before calling
`set_status`, and MUST call `set_status` with the span context captured by the
handler rather than the current span:

```erlang
Status = response_status(Meta),
run_hook(Hook, Context),
set_status(SpanCtx, Status),
otel_span:end_span(SpanCtx).
```

SDK status precedence, verified against `opentelemetry` 1.7.0:

| Status set first | Status set second | Resulting status |
| ---------------- | ----------------- | ---------------- |
| `error` | `error` | first `error`, with its description |
| `ok` | `error` | `ok` |
| unset | `error` | `error` |
| `error` | `ok` | `ok` |

A hook cannot return a span to unset. Where users need to suppress error status,
packages expose a configuration callback such as `error_status`, which the
instrumentation consults while computing its status.

See [Why status is set after the hook](../explanation/hooks.md#why-status-is-set-after-the-hook).

## Options That Accept Either A Value Or A Callback

When an option is useful both as a fixed value and as a computed one, both MUST
be accepted on the same option key, dispatching on `is_function/2`. Packages
MUST NOT introduce a parallel `_fn` suffixed key for the callback form.

`OpentelemetryAbsinthe`'s `error_status` option, which accepts either an atom or
a function, is the pattern to follow.

## Current State

Every callback-valued option in the repository today, measured against these
rules.

| Package | Option | Shape | Argument | Deviations |
| ------- | ------ | ----- | -------- | ---------- |
| bandit | `public_endpoint_fn` | `{M,F,A}` | `(conn, extra_args)` | shape, positional parameters |
| cowboy | `public_endpoint_fn` | `{M,F,A}` | `(Req, ExtraArgs)` | shape, positional parameters |
| bandit | `client_headers_sort_fn` | fun | two header values | comparator, compliant |
| bandit | `scheme_headers_sort_fn` | fun | two header values | comparator, compliant |
| bandit | `server_headers_sort_fn` | fun | two header values | comparator, compliant |
| cowboy | `client_headers_sort_fn` | fun | two header values | comparator, compliant |
| cowboy | `scheme_headers_sort_fn` | fun | two header values | comparator, compliant |
| cowboy | `server_address_headers_sort_fn` | fun | two header values | comparator, compliant |
| ecto | `telemetry_metadata_preprocessor` | fun | bare metadata map | not a context map |
| absinthe | `error_status` | fun or atom | bare error list | not a context map |
| xandra | `operation_parser` | fun | bare statement | not a context map, raises on bad return |
| httpoison | `infer_route` | fun | bare request struct | not a context map, read from application env |
| httpoison | `ot_resource_route` | fun, binary, or atom | bare request struct | not a context map, resolved per request |

No package wraps user-supplied code in `try`, so every option above can detach
its Telemetry handler.

`{M,F,A}` is used only by `public_endpoint_fn`.
