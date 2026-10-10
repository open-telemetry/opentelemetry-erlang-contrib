# Understanding Hooks and Callback Options

The reasoning behind the rules in
[Hooks and Callback Options](../reference/hooks.md).

## Why a Second Handler Cannot Work

It is tempting to let users attach their own `:telemetry` handler to the same
event and enrich the span from there. Each of the following is sufficient on
its own to rule that out.

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

## Why One Map

The single purpose of the context map is backward compatibility. A callback
signature is public API: it lives in user code, not in this repository, so every
change to its shape is a breaking change for every user who has written one.

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

The stability guarantees on the map are what make this work. They are the
reason the map exists, which is why they are binding rather than advisory.

Comparators are the exception because a comparison is definitionally binary and
cannot acquire further parameters.

## Why Nothing Is Promoted Out of the Map

`phase` is the recurring temptation, since it is present on every invocation and
exists purely for dispatch, which makes `hook(Phase, Context)` read well.

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
an API one. That is why package documentation leads with `phase` in its hook
examples.

## Why External Captures

External captures resolve by module and function name at call time, so they
survive code reload. A literal anonymous function pins the module version in
which it was created. This follows the guidance `telemetry` gives for its own
handlers.

With a context map there is no user-arguments slot, so `{M, F, A}` loses the one
advantage it had.

Measured dispatch cost, OTP 27.3, 20M iterations with an identical body:

| Form | Cost |
| ---- | ---- |
| `apply(M, F, Args)` | 10.7 ns/call |
| `fun mod:f/2` | 4.2 ns/call |
| `fun(X) -> ... end` | 4.5 ns/call |

The absolute difference is negligible against span creation. Reload safety and
consistency with `telemetry`, not speed, are the reasons for the rule.

## Why Error Containment Is Mandatory

Containment is not defensive style, it is required for correctness. From the
`telemetry` documentation: "If the function fails (raises, exits or throws) then
the handler is removed and a failure event is emitted." An unguarded callback
that raises does not lose one attribute. It detaches the handler, silently
disabling all instrumentation from that package for the lifetime of the node.
Raising on a bad return value triggers the same detach.

Metadata shape varying by event makes this likely rather than theoretical. A
callback matching `#{meta := #{req := Req}}` on `opentelemetry_cowboy` raises on
every `[cowboy, request, early_error]`, whose metadata carries `partial_req`
instead.

Because no package in the repository wraps user code today, containment is the
highest priority fix.

## Why a Single Hook Option

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

## Why Exceptions Are a Stop Phase

An exception is a span ending, so a hook that matches `phase := stop` to record
end-of-request state is called for it. Failed requests are the ones whose
attributes matter most, and a separate phase would make covering them something
the user has to remember rather than the default. Users who want to act only on
failure match `event`, or the reason keys in `meta`.

`early_error` is invoked as `start` then `stop` for the same reason. A hook
matching `phase := start` is called for malformed requests without the user
special casing an event whose metadata carries `partial_req` instead of `req`.

## Why a Catch-All Clause

One hook receives every event. Without a catch-all, it raises `function_clause`
on every event it does not match, including events added in later releases.
Error containment turns that into a logged no-op rather than an outage, but the
log volume makes it the user's problem to avoid.

## Sampling and Hooks

A hook runs too late to affect head sampling. For application-specific values
that should drive sampling, users either tail-sample in the Collector, which
sees the complete span, or supply the value before span creation by a means
outside the instrumentation's handler.

## Why Status Is Set After the Hook

Attributes follow last-write-wins, so the hook overrides instrumented attributes
simply by running after them. Span status works the other way. The SDK ignores a
second `error` once one is set and treats `ok` as final, so the first write
decides.

Running the hook before the instrumentation's `set_status` uses that to give the
hook the last word without any coordination. The package always applies the
status it computed. If the hook already set one, the SDK drops the package's
write; if not, the package's status lands. The package never needs to know what
the hook did, and the API offers no way to read status back anyway.

Reversing the order silently weakens the hook. With the package's `error`
already stored, the hook can only replace it with `ok`, never with its own
description, and nothing reports the loss.

The span context is captured by the handler because the hook may change the
current span, for example by starting a child span. Setting status through the
current context would then mark the wrong span.

Unset is the one outcome a hook cannot produce. Once `error` is stored nothing
returns it to unset, and `ok` is not a substitute because it asserts success
rather than the absence of a judgement. Suppressing error status is therefore a
decision the instrumentation has to make while computing its status, which is
what a configuration callback such as `error_status` is for.
