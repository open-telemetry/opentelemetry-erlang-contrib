# Documentation

## How-to guides

Task-oriented guides:

- [Record Exceptions](how-to/record-exceptions.md): recording exception events, span status, and `error.type` from Telemetry handlers and non-Telemetry code paths

## Reference

Technical specifications:

- [Hooks and Callback Options](reference/hooks.md): why user code must be a `setup/1` option rather than a second `:telemetry` handler, callback shape and arity, error containment, and hook precedence
- [Configuration Options](reference/configuration-options.md): standard option types, defaults, naming conventions, SemConv requirement level mapping, and package support matrix
- [Custom Span Attributes](reference/custom-span-attributes.md): how to define package-specific span attributes using a `[Component]Attributes` module
- [Recording Exceptions](reference/recording-exceptions.md): exception event fields, span status description strings, `error.type` values, and `erlang.exception.kind`
- [Context Propagation Precedence](reference/context-propagation.md): why `:telemetry` handlers must check for an already-attached context before falling back to a `$callers` ancestor
