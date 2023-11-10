# Changelog

## 0.2.0

### Fixes

* Add support for Req v0.4

* Change http.url to follow [OpenTelemetry http spec](https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/http/#http-client).

* Full HTTP request URL in the form `scheme://host[:port]/path?query[#fragment]`

* Strip user credentials passed via URL

## 0.1.2

### Fixes

* Fix ctx not being set back to parent upon completion

## 0.1.1

### Fixes

* Fix client span to be the ctx injected to headers

## 0.1.0

### Features

* Initial release

