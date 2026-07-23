# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

---

## [2.3.0] - 2024-05-27

### Added

- Support custom metadata from integrators. Use `OpentelemetryAbsinthe.TelemetryMetadata` to add metadata to your context which will then be broadcast.
- Allow attaching to `[:absinthe, :subscription, :publish]` (both `:start` and `:stop`) given a `trace_subscription: true` config
- New `graphql.event.type` trace attribute, with value `operation` or `publish`

## [2.3.0-rc.0] - 2024-04-18

### Added

- Allow attaching to `[:absinthe, :subscription, :publish]` (both `:start` and `:stop`) given a `trace_subscription: true` config
- New `graphql.event.type` trace attribute, with value `operation` or `publish`

## [2.2.1] - 2024-02-21

### Changed

- Unpinned absinthe patch version in order to not downgrade it when this package is required

## [2.2.0] - 2024-02-20

### Changed

- included errors in graphql telemetry events

## [2.1.0] - 2024-01-12

### Changed

- dispatch telemetry events for the handling of graphql requests

## [2.0.1] - 2023-03-14

### Changed

- absinthe is now a required dependency. The library failed to compile since version 2.0.0 with it being optional so this is not a breaking change.
- attribute keys are now atoms, which should offer minor performance improvements.
- loosened the telemetry version requirement from `~> 0.4 or ~> 1.0.0` to `~> 0.4 or ~> 1.0`

## [2.0.0] - 2023-03-07

### Added

- new `trace_request_selections` option to enable tracing root level GraphQL selections, which will be stored under `graphql.request.selections`.
- attribute `graphql.operation.name` was added.
- attribute `graphql.operation.type` was added.
- span_name can now be set to `:dynamic`, causing it to be set dynamically based on the operation type and name, as recommended by [opentelemetry](https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/instrumentation/graphql/).

### Changed

- BREAKING: `graphql.request.query` was renamed to `graphql.document`.
- BREAKING: the default value of span_name is now `:dynamic`
- BREAKING: opentelemetry_absinthe will no longer log sensitive information by default.
  By default the graphql.request.variables, graphql.response.errors and graphql.response.result attributes will no longer be emited.
  The previous behavior can be restored by setting the opentelemetry_absinthe configuration options.

- `OpentelemetryAbsinthe.setup` can now optionally recieve the configuration. Previously `OpentelemetryAbsinthe.Instrumentation.setup` had to be used.

### Deprecated
- setting the span name to a static string.

## [1.1.0] - 2022-09-21

### Changed

- opentelemetry_absinthe does not set opentelemetry-related Logger metadata anymore, because
  The OpenTelemetry API/SDK itself [does that automatically since 1.1.0](https://github.com/open-telemetry/opentelemetry-erlang/pull/394).
  If you're upgrading to opentelemetry_absinthe 1.1.0, it is therefore recommended to also upgrade to OpenTelemetry API 1.1.0
  in order to keep the opentelemetry log metadata.
