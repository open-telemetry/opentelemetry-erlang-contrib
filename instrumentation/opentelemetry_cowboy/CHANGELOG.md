# Changelog

## Unreleased

### Added

* New options (`req_headers_to_span_attributes`  and `resp_headers_to_span_attributes`) to automatically add request/response headers as attributes 

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
