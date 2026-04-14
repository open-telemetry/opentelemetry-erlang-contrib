# Custom Span Attributes

When an instrumentation package emits span attributes that are not (yet) part of
the [OpenTelemetry Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/),
those attributes must be defined in a dedicated `[Component]Attributes` module
inside the package. This mirrors how `opentelemetry_semantic_conventions` exposes
official attributes, so consumers have a single, stable place to reference every
attribute key a package can emit.

## Why a Dedicated Module

- **Discoverability** — one module lists every non-SemConv attribute the package
  can produce. No need to grep for string literals.
- **Consistency** — callers use the same function calls regardless of whether an
  attribute is from SemConv or custom to the package.
- **Compile-time safety** — calling a function like `MylibAttributes.mylib_queue_name()`
  means a typo is an undefined function error caught by the compiler, not a silent
  wrong atom at runtime.
- **Future migration** — when an attribute is eventually standardised in SemConv,
  the change is made in one place and callers are updated by their compiler.

## Module Naming Convention

| Package | Module |
| ------- | ------ |
| `opentelemetry_ecto` | `OpentelemetryEcto.EctoAttributes` |
| `opentelemetry_nebulex` | `OpentelemetryNebulex.NebulexAttributes` |
| `opentelemetry_mylib` | `OpentelemetryMylib.MylibAttributes` |

The module lives inside the package's `lib/` tree. For small packages a top-level
module is fine; for packages with multiple sub-components, nest it under the
relevant namespace (e.g. `OpentelemetryNebulex.NebulexAttributes`).

## Attribute Key Naming Convention

- Prefix every key with the component name to avoid collisions, using dot
  notation: `mylib.`, `mylib.operation.`, etc.
- Use `snake_case` segments separated by `.`.
- Do **not** start with `otel.` or reuse any existing SemConv namespace unless
  the attribute is intentionally extending that namespace.

```
mylib.cache          ✓
mylib.queue_name     ✓
mylib.retry_count    ✓
otel.mylib.cache     ✗  (otel. prefix is reserved)
cache                ✗  (no namespace)
```

## Defining the Module

Each function returns the atom key for one attribute. The `@spec` and `@doc`
document the expected value type and semantics — the same style used by
`opentelemetry_semantic_conventions`.

```elixir
defmodule OpentelemetryMylib.MylibAttributes do
  @moduledoc """
  OpenTelemetry span attributes specific to the Mylib instrumentation.

  These attributes are not part of the OpenTelemetry Semantic Conventions.
  """

  @doc """
  The name of the Mylib queue being processed.
  """
  @spec mylib_queue_name() :: :"mylib.queue_name"
  def mylib_queue_name, do: :"mylib.queue_name"

  @doc """
  Number of times the operation was retried.
  """
  @spec mylib_retry_count() :: :"mylib.retry_count"
  def mylib_retry_count, do: :"mylib.retry_count"
end
```

## Using the Module

### At runtime

Call the function when building the attributes map or setting attributes on a
span:

```elixir
alias OpentelemetryMylib.MylibAttributes

attributes = %{
  MylibAttributes.mylib_queue_name() => queue_name,
  MylibAttributes.mylib_retry_count() => retries
}
```

