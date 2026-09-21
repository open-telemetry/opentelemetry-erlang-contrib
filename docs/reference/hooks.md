# Hooks and Callback Options

Rules for any configuration option whose value is user-supplied code. These
apply to every instrumentation package in this repository, not only HTTP ones.

## Callbacks Must Be Setup-Time Options

User code that needs to participate in span creation MUST be passed as an
option to the package's `setup/1`. It MUST NOT be expected to work as a second
`:telemetry` handler attached to the same event.

Three independent reasons, each sufficient on its own:

**Handler order is undefined.** From the `telemetry` documentation: "Note that
you should not rely on the order in which handlers are invoked." A user handler
cannot be sequenced relative to an instrumentation handler.

**The span is created inside the instrumentation handler.** By the time any
other handler for the same event runs, the span exists and the sampling
decision is final. Event metadata is immutable and identical for every handler,
so there is no channel through which a second handler can contribute.

**The span may not be reachable from user code at all.** Span context is stored
in the process dictionary, so it is process-local. For `opentelemetry_cowboy`
the span is created in the connection process by `cowboy_telemetry_h:init/3`,
while the user's cowboy handler runs in a separate request process spawned by
`cowboy_stream_h:init/3`. Calling `?set_attributes` from a cowboy handler
therefore has no effect on the server span.

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
a trailing user-arguments slot.

### Rationale: Avoiding Breaking Changes

The single purpose of this rule is backward compatibility. A callback signature
is public API: it lives in user code, not in this repository, so every change to
its shape is a breaking change for every user who has written one.

Positional parameters cannot grow. Passing any new piece of information means a
new parameter, which means a new arity, which means every existing callback
stops being callable:

```erlang
%% today
is_public(Req, ExtraArgs) -> ...

%% instrumentation now also needs the resolved config, so arity changes
%% and every user callback written against the old arity breaks
is_public(Req, Config, ExtraArgs) -> ...
```

A context map grows without breaking anything, because map patterns in both
Erlang and Elixir match partially. A callback matches the keys it cares about
and ignores the rest, so new keys are invisible to it:

```erlang
%% written today, matches one key
hook(#{meta := Meta}) -> ...

%% instrumentation adds `config` and `span_ctx` in a later release.
%% The callback above is untouched and keeps working.
```

`public_endpoint_fn` is the cautionary case already in the repository. It is
defined as `(Req, ExtraArgs)`, having grown a second parameter purely to carry
user state that an `{M, F, A}` tuple could not close over. Anything further it
needs requires a third parameter and a breaking release.

### Nothing Is Promoted Out Of The Map

Values that look like natural first arguments stay inside the map. `phase` is
the recurring temptation, since it is present on every invocation and exists
purely for dispatch, which makes `hook(Phase, Context)` read well.

`telemetry` is the reason not to. Its handlers are
`handle_event(Event, Measurements, Metadata, Config)`, four positional
parameters fixed for the life of the library. Everything the ecosystem has
needed to pass since then goes into `Metadata`, because changing the arity would
break every handler in existence. The positional parameters are now a record of
what happened to be known when the signature was chosen, and the map absorbs all
growth regardless.

Promoting a value produces that outcome without avoiding the map. It also has no
obvious stopping point: `event` is equally always present and equally exists for
dispatch, so any rule admitting `phase` admits `event`, and the line between
promoted and not becomes the next breaking change.

The capability is identical either way, because map patterns match in the
function head:

```erlang
hook(start, #{meta := #{req := Req}}) -> ...       % not this
hook(#{phase := start, meta := #{req := Req}}) -> ...  % this
```

Discoverability is the real cost, and it is a documentation problem rather than
an API one. Package documentation MUST lead with `phase` in its hook examples so
the dispatch key is the first thing a reader sees.

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

These guarantees are what make the context map safe to evolve. They are the
reason the map exists, so they are binding rather than advisory.

The context map is additive-only. Packages MAY add keys in a minor release.
Packages MUST NOT remove or rename a key outside a major release, and MUST NOT
reorder anything, since there is no order to depend on.

Callbacks SHOULD match only the keys they use. Callers MUST NOT depend on the
map's exact key set, for example by enumerating keys or checking its size.

The same applies to values. Packages MAY add `event` and `phase` values in a
minor release, so callbacks MUST NOT assume the sets are closed.

### Comparators

Callbacks that compare two values of the same kind, such as the header sort
functions consumed by `otel_http`, take those two values positionally. A
comparison is definitionally binary and cannot acquire further parameters, so
the context map does not apply.

## Callback Shape

Callbacks MUST accept an external function capture:

```erlang
setup(#{hook => fun my_mod:hook/1})
```

```elixir
setup(hook: &MyMod.hook/1)
```

Packages SHOULD NOT accept `{M, F, A}` tuples for new options, and SHOULD NOT
require literal anonymous functions. This follows the guidance `telemetry`
gives for its own handlers: prefer external captures, avoid literal anonymous
functions and local captures such as `fun on_start/1`.

External captures resolve by module and function name at call time, so they
survive code reload. A literal anonymous function pins the module version in
which it was created.

With a context map there is no user-arguments slot, so `{M, F, A}` loses the one
advantage it had. User state belongs in the callback's own module or in
`config`.

Measured dispatch cost, OTP 27.3, 20M iterations with an identical body:

| Form | Cost |
| ---- | ---- |
| `apply(M, F, Args)` | 10.7 ns/call |
| `fun mod:f/2` | 4.2 ns/call |
| `fun(X) -> ... end` | 4.5 ns/call |

The absolute difference is negligible against span creation. Reload safety and
consistency with `telemetry`, not speed, are the reasons for this rule.

## Metadata Shape Varies By Event

Documentation for each hook MUST state the shape of `meta` per event, because it
differs between events on the same package. In `opentelemetry_cowboy`,
`[cowboy, request, start]` metadata carries `req`, while
`[cowboy, request, early_error]` metadata carries `partial_req` and no `req`
key. A callback matching `#{meta := #{req := Req}}` raises on every early error.

This is why error containment below is mandatory rather than advisory.

## Resolution Time

Callbacks and all other options MUST be resolved once during `setup/1` and
stored in the handler configuration passed to `telemetry:attach_many/4`.

Packages MUST NOT read `application:get_env`, `Application.get_env`, or
`System.get_env` per request or per event, and MUST NOT re-run option
validation per request.

## Error Containment

Every invocation of user-supplied code MUST be wrapped so that an exception
cannot escape into the Telemetry handler.

This is not defensive style, it is required for correctness. From the
`telemetry` documentation: "If the function fails (raises, exits or throws) then
the handler is removed and a failure event is emitted." An unguarded callback
that raises does not lose one attribute. It detaches the handler, silently
disabling all instrumentation from that package for the lifetime of the node.

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
and fall back to the default. Packages MUST NOT raise on a bad return value,
because raising inside a handler triggers the detach described above.

## The Hook Option

Packages expose exactly one hook option, named `hook`. It is invoked at every
span lifecycle point the package instruments, runs with the span active, and its
return value is ignored. Inside it, users may call `set_attributes`, `add_event`,
`update_name`, or any other span operation.

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

### Rationale: One Option Does Not Grow

This is the context map argument one level up. With per-phase options, every new
lifecycle point is a new option, and a new option is API surface that all
packages then have to mirror and document. With a single `hook`, a new lifecycle
point is a new `event` and `phase` value, which is additive and costs no API.

Per-phase names also promise a uniformity that does not exist. A package may
instrument several events that all start a span: `opentelemetry_cowboy` starts
one from `[cowboy, request, start]` and another from
`[cowboy, request, early_error]`, whose metadata carries `partial_req` rather
than `req`. A `span_start_hook` would still be called with both shapes, so the
user must dispatch on the event regardless. Naming hooks by phase partitions the
problem without solving it.

### Phase And Event

`event` is the package's own Telemetry event name. `phase` is the normalized
span lifecycle moment and is only ever `start` or `stop`. Packages MUST map
every instrumented event onto a phase.

`opentelemetry_cowboy`:

| Event | Phase | Notes |
| ----- | ----- | ----- |
| `[cowboy, request, start]` | `start` | |
| `[cowboy, request, stop]` | `stop` | |
| `[cowboy, request, exception]` | `stop` | `meta` carries `kind`, `reason`, and `stacktrace` |
| `[cowboy, request, early_error]` | `start`, then `stop` | the span is created and ended inside one handler call, so the hook is invoked twice |

There is no `exception` phase. An exception is a span ending, so a hook that
matches `phase := stop` to record end-of-request state is called for it. Failed
requests are the ones whose attributes matter most, and a separate phase would
make covering them something the user has to remember rather than the default.
Users who want to act only on failure match `event`, or the reason keys in
`meta`.

`early_error` is invoked twice for the same reason. A hook matching
`phase := start` is called for malformed requests without the user special
casing an event whose metadata carries `partial_req` instead of `req`.

### A Catch-All Clause Is Required

Because one hook receives every event, user hooks MUST end with a clause that
matches any context:

```erlang
hook(_Context) -> ok.
```

Without it, a hook raises `function_clause` on every event it does not match,
including events added in later releases. Error containment turns that into a
logged no-op rather than an outage, but the log volume makes it the user's
problem to avoid.

Package documentation MUST show the catch-all clause in its hook examples.

### Hooks Are Not Configuration Callbacks

A hook produces side effects and its return value is ignored. A configuration
callback returns a value the instrumentation acts on, such as
`public_endpoint_fn` returning a boolean or `operation_parser` returning a
parsed tuple.

These are different families. Configuration callbacks keep their own descriptive
option names and are not folded into `hook`. Both follow the context map and
shape rules above.

### Hooks Cannot Influence Sampling

A hook runs after the span exists, therefore after the head sampling decision.
Samplers receive only the initial attribute set passed at span creation, so
attributes set from a hook are invisible to them.

Attributes needed for a head sampling decision must be supplied at creation
time. Where the HTTP semantic conventions list attributes as sampling-relevant,
instrumentation already provides them at creation time. For application-specific
values, users should either tail-sample in the Collector, which sees the
complete span, or supply the value before span creation by a means outside the
instrumentation's handler.

### Hooks Can Overwrite Instrumented Attributes

A hook runs after the instrumentation has set its own attributes, so a
`set_attributes` call naming the same key wins. This is the opposite of the
precedence rule for `extra_attrs`, where instrumented attributes take
precedence.

Hook documentation MUST warn against writing keys the instrumentation already
owns, and MUST point users at their own attribute namespace. Per the
OpenTelemetry attribute naming guidance, the `otel.*` namespace is reserved and
existing semantic convention namespaces should not be reused. Application
attributes belong under a reverse domain name or an application prefix, for
example `com.acme.tenant`.

## Options That Accept Either A Value Or A Callback

When an option is useful both as a fixed value and as a computed one, both MUST
be accepted on the same option key, dispatching on `is_function/2`. Packages
MUST NOT introduce a parallel `_fn` suffixed key for the callback form.

`OpentelemetryAbsinthe`'s `error_status` option, which accepts either an atom or
a function, is the pattern to follow.

## Current State

Every callback-valued option in the repository today, measured against the rules
above.

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

No package wraps user-supplied code in `try`. All of the above can therefore
detach their Telemetry handler and silently disable instrumentation, which makes
error containment the highest priority fix.

`{M,F,A}` is used only by `public_endpoint_fn`. Migrating those two options to an
external capture taking a context map would leave the repository with a single
callback convention.
