# Context propagation precedence

Specification for resolving the parent OTel context in `:telemetry` handlers
that run in a different process than the one that emitted the event.

## Mechanisms

| Mechanism | Role |
| --------- | ---- |
| `OpentelemetryProcessPropagator.fetch_ctx/1` | Reads the OTel context already attached to a given pid's process dictionary (`$__current_otel_ctx`). |
| `OpentelemetryProcessPropagator.fetch_parent_ctx/2` | Walks the `$callers` (or other configured key) process-dictionary chain, up to `MaxDepth` ancestors, and returns the first attached context found. |
| `OpenTelemetry.Ctx.attach/1` | Makes a context current for the calling process; returns a token for detaching later. |

## The precedence guard

A `:telemetry` handler runs in whatever process emitted the event. That
process may already have its own context attached — for example, another
instrumentation package already started a span there. `fetch_parent_ctx/2`
does not know about that: it only walks `$callers`, so it can return a more
distant ancestor's context even when a closer, already-current one exists.

Handlers that call `fetch_parent_ctx/2` unconditionally therefore risk
overwriting a closer, correct parent with a more distant one, misparenting
the span it's about to start. The guard checks `fetch_ctx(self())` first and
only falls back to the `$callers` walk when nothing is attached locally:

```elixir
parent_ctx =
  case OpentelemetryProcessPropagator.fetch_ctx(self()) do
    :undefined -> OpentelemetryProcessPropagator.fetch_parent_ctx(depth, :"$callers")
    ctx -> ctx
  end

if parent_ctx != :undefined do
  OpenTelemetry.Ctx.attach(parent_ctx)
end
```

## Packages implementing the guard

| Package | `MaxDepth` | Handler(s) |
| ------- | ---------- | ---------- |
| `opentelemetry_ecto` | 1 | query span start |
| `opentelemetry_redix` | 1 | command span start |
| `opentelemetry_dataloader` | 4 | `run` and `batch` span start |

## Failure mode without the guard

Concretely: a process starts a span (e.g. Absinthe resolving a field), then
calls into Dataloader, which schedules work through `$callers`-tracked
processes. A handler that calls `fetch_parent_ctx/2` unconditionally
overwrites the Absinthe-attached context with whatever it finds walking
`$callers`, so the resulting span attaches to the wrong ancestor
(`Phoenix -> Dataloader` instead of `Phoenix -> Absinthe -> Dataloader`).
The guard preserves the closer, already-attached context instead.
