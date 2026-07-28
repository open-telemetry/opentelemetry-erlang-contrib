# How to record exceptions on spans

This guide covers recording failures on spans in instrumentation packages.
For the API specifications, see [Recording Exceptions](../reference/recording-exceptions.md).

## `record_exception` and `set_status` are complementary

Both calls are required when recording a failure. They serve different purposes:

- **`record_exception`** adds an **event** to the span with structured fields
  (`exception.type`, `exception.message`, `exception.stacktrace`). Trace
  backends use this for detailed error inspection.
- **`set_status(:error, description)`** marks the **span status** as errored
  with a human-readable description. Dashboards and trace UIs use this to
  quickly identify failed spans.

One does not replace the other — `record_exception` without `set_status` leaves
the span looking successful, and `set_status` without `record_exception` loses
the structured exception detail.

## Handling a `:telemetry` `[:*, :exception]` event

You have `%{kind: kind, reason: reason, stacktrace: stacktrace}`.

**When `reason` is likely an Elixir exception struct:**

```elixir
ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

exception = Exception.normalize(kind, reason, stacktrace)

OpenTelemetry.Span.record_exception(ctx, exception, stacktrace, [])
OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, Exception.format_banner(kind, reason, stacktrace)))

OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
```

When the exception event already carries sufficient detail, an empty status
description is also acceptable (used in `opentelemetry_phoenix`):

```elixir
OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, ""))
```

**When `reason` may be any term:**

```elixir
ctx = OpentelemetryTelemetry.set_current_telemetry_span(@tracer_id, meta)

:otel_span.record_exception(ctx, kind, reason, stacktrace, [])
OpenTelemetry.Span.set_status(ctx, OpenTelemetry.status(:error, Exception.format_banner(kind, reason, stacktrace)))

OpentelemetryTelemetry.end_telemetry_span(@tracer_id, meta)
```

## Handling a non-Telemetry failure

When you hold the error directly from a `{:error, reason}` return or a
`rescue`/`catch` block, define a small helper:

```elixir
defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
defp format_error(reason), do: inspect(reason)
```

Then record and set status:

```elixir
OpenTelemetry.Span.record_exception(ctx, exception, stacktrace, [])
OpenTelemetry.Span.set_status(span, OpenTelemetry.status(:error, format_error(reason)))
```

## Setting `error.type`

Set `error.type` on the span when the operation failed; omit it on success.
Choose a value that stays low-cardinality:

```elixir
# HTTP status
OpenTelemetry.Span.set_attribute(ctx, ErrorAttributes.error_type(), to_string(status_code))

# Named exception
OpenTelemetry.Span.set_attribute(ctx, ErrorAttributes.error_type(), inspect(exception.__struct__))

# Unknown / opaque
OpenTelemetry.Span.set_attribute(ctx, ErrorAttributes.error_type(), ErrorAttributes.error_type_values().other)
```

`error.type` summarises the failure class for dashboards; it does not replace
`exception.*` fields on the event.

## Recording the OTP exception class as an attribute

Add `:"erlang.exception.kind"` only when backends or queries need the class as
a structured field separate from the exception event:

```elixir
OpenTelemetry.Span.set_attribute(ctx, :"erlang.exception.kind", kind)
```

Pick one value shape per package (atom or string) and document it.

## Erlang handlers

Call `otel_span:record_exception/5` and set status with
`opentelemetry:status(?OTEL_STATUS_ERROR, Message)` where `Message` is a UTF-8
binary.

## Checklist for new instrumentation

1. Add an **exception event** via `OpenTelemetry.Span.record_exception/4`
   (exception struct) or `:otel_span.record_exception/5` (raw kind/reason).
2. Set **span status** to `OpenTelemetry.status(:error, description)` with a
   concise description.
3. When SemConv applies, set **`error.type`** with a low-cardinality value.
4. Optionally set **`erlang.exception.kind`** if consumers need the OTP class
   as a separate attribute.
