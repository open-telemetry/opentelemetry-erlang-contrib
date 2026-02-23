# Configuration Options Reference

Standard configuration options for instrumentation packages in this repository.
All options use [NimbleOptions](https://hexdocs.pm/nimble_options) for
validation and documentation generation.

## `opt_in_attrs`

Enables attributes with a requirement level of **Opt-In** or **Experimental**
per the [OpenTelemetry Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/).

| Property | Value                                                                             |
| -------- | --------------------------------------------------------------------------------- |
| Type     | `{:list, {:in, opt_ins}}` where `opt_ins` is the list of supported attribute keys |
| Default  | `[]`                                                                              |
| Name     | `opt_in_attrs`                                                                    |

Attribute keys must come from the `opentelemetry_semantic_conventions` library.

## `opt_out_attrs`

Disables attributes with a requirement level of **Recommended** that are
included by default.

| Property | Value                                                                                                      |
| -------- | ---------------------------------------------------------------------------------------------------------- |
| Type     | `{:list, {:in, opt_outs}}` where `opt_outs` is the list of recommended attribute keys that can be disabled |
| Default  | `[]`                                                                                                       |
| Name     | `opt_out_attrs`                                                                                            |

## `extra_attrs`

User-defined attributes included on all spans. Instrumented attributes take
precedence over extra attributes.

| Property | Value                                     |
| -------- | ----------------------------------------- |
| Type     | `:map` (`OpenTelemetry.attributes_map()`) |
| Default  | `%{}`                                     |
| Name     | `extra_attrs`                             |

> `opentelemetry_ecto` uses `additional_span_attributes` for historical reasons.
> New packages must use `extra_attrs`.

## `span_relationship`

Controls how spans relate to a propagated parent context in asynchronous
processing.

| Property | Value                                                       |
| -------- | ----------------------------------------------------------- |
| Type     | `{:in, [:child, :link, :none]}`                             |
| Default  | Varies by package (`:link` recommended for async consumers) |

| Value    | Behavior                                                |
| -------- | ------------------------------------------------------- |
| `:child` | Attach propagated context, creating a parent-child span |
| `:link`  | Create a span link to the propagated context            |
| `:none`  | Ignore propagated context entirely                      |

## Module Names in Attribute Values

When an attribute value is an Elixir module name, use the short form without the
`Elixir.` prefix. The `Elixir.` prefix is a BEAM-internal detail and must not
leak into telemetry data.

| Form                | Example             | Status     |
| ------------------- | ------------------- | ---------- |
| Short (no prefix)   | `GRPC.RPCError`     | **Use**    |
| Prefixed            | `Elixir.GRPC.RPCError` | **Avoid** |

## Semantic Convention Requirement Levels

The [OpenTelemetry Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/general/attribute-requirement-level/)
define requirement levels for each attribute. This repository maps those levels
to configuration options as follows:

| SemConv Level | Instrumentation Behavior | User Control    |
| ------------- | ------------------------ | --------------- |
| Required      | Always included          | None            |
| Recommended   | Included by default      | `opt_out_attrs` |
| Opt-In        | Excluded by default      | `opt_in_attrs`  |
| Experimental  | Excluded by default      | `opt_in_attrs`  |
