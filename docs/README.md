# Documentation

## How-to guides

Task-oriented guides:

- [Record Exceptions](how-to/record-exceptions.md): recording exception events, span status, and `error.type` from Telemetry handlers and non-Telemetry code paths
- [Release a Package to Hex](how-to/release-a-package.md): preparing the version pull request, publishing, changing the version, and recovering from a failed release
- [Add a Package to the Release Workflows](how-to/add-a-package-to-the-release-workflows.md): workflow configuration, Hex ownership, and the starting version for a package that is already on Hex

## Reference

Technical specifications:

- [Hooks and Callback Options](reference/hooks.md): rules for callback-valued options, context map keys, callback shape, error containment, the `hook` option, and attribute and span status precedence
- [Configuration Options](reference/configuration-options.md): standard option types, defaults, naming conventions, SemConv requirement level mapping, and package support matrix
- [Custom Span Attributes](reference/custom-span-attributes.md): how to define package-specific span attributes using a `[Component]Attributes` module
- [Recording Exceptions](reference/recording-exceptions.md): exception event fields, span status description strings, `error.type` values, and `erlang.exception.kind`
- [Context Propagation Precedence](reference/context-propagation.md): why `:telemetry` handlers must check for an already-attached context before falling back to a `$callers` ancestor

## Explanation

Background and design rationale:

- [Understanding Hooks and Callback Options](explanation/hooks.md): why user code must be a `setup/1` option rather than a second `:telemetry` handler, why callbacks take one map, and why span status is set after the hook
- [Understanding the Release Process](explanation/releases.md): how Release Drafter and Publish Hex Release fit together, why the draft decides the version, why publishing takes two runs, and who can publish
