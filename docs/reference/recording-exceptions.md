# Recording exceptions and errors

Specification for recording failures on spans in **opentelemetry-erlang-contrib**
instrumentations.

See [How to record exceptions](../how-to/record-exceptions.md) for usage guidance.

## Mechanisms

| Mechanism | Role |
| --------- | ---- |
| **Exception event** (`exception` / `:exception`) | Captures the failure for the trace: `exception.type`, optional `exception.message`, `exception.stacktrace`. |
| **Span status** (`OpenTelemetry.status/2`) | Marks the span outcome: `:error` with a short description, or `:ok` / unset. |
| **`error.type` span attribute** | Stable SemConv attribute for error classification; low-cardinality. Key: `OpenTelemetry.SemConv.ErrorAttributes.error_type/0`. |
| **`erlang.exception.kind`** | Optional contrib-local attribute for the OTP exception class. Not part of stable SemConv. |

## `Span.record_exception`

### Elixir: `OpenTelemetry.Span.record_exception/4`

```
OpenTelemetry.Span.record_exception(span_ctx, exception, stacktrace \\ nil, attributes \\ [])
```

Requires `exception` to be a struct with `__exception__: true`. Returns `false`
and adds no event otherwise.

Produces an `exception` event with:

| Field | Value |
| ----- | ----- |
| `exception.type` | `to_string(exception.__struct__)` |
| `exception.message` | `Exception.message(exception)` |
| `exception.stacktrace` | `Exception.format_stacktrace(stacktrace)` |

### Erlang: `:otel_span.record_exception/5` and `/6`

```
:otel_span.record_exception(Ctx, Class, Reason, Stacktrace, Attributes)
:otel_span.record_exception(Ctx, Class, Reason, Stacktrace, Message, Attributes)
```

Accepts any `Reason` term (atoms, tuples, bare terms). `Class` is the OTP class:
`error`, `exit`, or `throw`.

Produces an `exception` event with:

| Field | Value |
| ----- | ----- |
| `exception.type` | Encoded `Class:Reason` string (limited print depth/length) |
| `exception.stacktrace` | Formatted stacktrace |
| `exception.message` | `/6` only; caller-supplied binary |

## `Exception.normalize/3`

```
Exception.normalize(kind, reason, stacktrace) :: Exception.t() | term()
```

Maps OTP `kind`/`reason` pairs to an Elixir exception struct where possible.
Result is suitable for `Exception.message/1` and
`OpenTelemetry.Span.record_exception/4` when the return value is an exception
struct.

## Span status description strings

| Source | Expression |
| ------ | ---------- |
| Telemetry triple | `Exception.format_banner(kind, reason, stacktrace)` |
| Exception struct | `Exception.message(exception)` |
| Opaque reason | `inspect(reason)` |
| Domain-specific | HTTP status string, gRPC message, etc. |

## `ErrorAttributes.error_type/0` (`error.type`)

`OpenTelemetry.SemConv.ErrorAttributes.error_type/0` returns `:"error.type"`.

| Situation | Example values |
| --------- | -------------- |
| HTTP-like status | `to_string(404)`, `to_string(500)` |
| Named exception module | `inspect(MyApp.SomeError)`, `to_string(GRPC.RPCError)` |
| DB / driver codes | Postgres/MySQL/TDS numeric codes |
| Opaque / unknown | `ErrorAttributes.error_type_values().other` (`:_OTHER`) |
| Transport / errno-style | `inspect(:econnrefused)` |

Set on the span when the operation failed; omit on success.

## `erlang.exception.kind`

Key: `:"erlang.exception.kind"`

Values: `error`, `exit`, or `throw`. Use atoms or short strings; pick one
shape per package and document it.

Not required when using `:otel_span.record_exception/5`: the OTP class is
already encoded into `exception.type` on the event. Use this attribute only
when consumers need the class as a structured, low-cardinality field.
