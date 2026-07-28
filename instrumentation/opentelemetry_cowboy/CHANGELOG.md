# Changelog

# v1.0.0

NOTE: This release includes numerous breaking changes with implementation
of Semantic Conventions introduced in v1.20. The full list of changes
are enumerated in the [HTTP Stability Migration Guide](https://opentelemetry.io/docs/specs/semconv/non-normative/http-migration/).

### Changed

* Semantic Conventions v1.26.0 compliance
* Added public endpoint settings for determining whether to continue a trace or create a link

## v0.3.0

### Changed

* Dependency maintenance release

## v0.2.1

### Fixes

* Set span kind as server

## v0.2.0

### Fixes

* Handle binary resp_status from Cowboy
* Fix status code attribute naming to match spec
* Only mark 5xx level status codes as errored

## v0.2.0-beta.1

### Changed

* Opentelemetry 1.0 support
