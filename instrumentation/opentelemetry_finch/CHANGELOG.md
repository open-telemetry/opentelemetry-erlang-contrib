# Changelog

## Unreleased

### Changed

* Replace deprecated `OpenTelemetry.SemanticConventions.Trace` attributes with `opentelemetry_semantic_conventions` >= v1.27.0 modules (`HTTPAttributes`, `ServerAttributes`, `URLAttributes`)

### Fixed

* Leave the span status description unset for 4xx and 5xx responses, since the reason is inferable from `http.response.status_code`

## 0.3.0

### Changed

* Update OpenTelemetry API and Semantic Conventions

## 0.2.0

* Span attributes update

## 0.1.0

* Initial release
