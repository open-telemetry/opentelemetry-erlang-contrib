# Changelog

## v0.2.0

### Breaking

* Package renamed to `otel_http` from `opentelemetry_instrumentation_http`

### Features

* Adds several header extraction and manipulation functions for common tasks
  in instrumentation libraries:
  * `extract_client_info/1`
  * `extract_client_info/2`
  * `extract_scheme/1`
  * `extract_scheme/2`
  * `extract_server_info/1`
  * `extract_server_info/2`
* Generate OTP27 docs

## v0.1.0

### Changed

* Expose `extract_headers_attributes/3` and `normalize_header_name/1` functions to extracted selected headers in an attribute map following semantic conventions
